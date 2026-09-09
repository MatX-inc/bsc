#!/usr/bin/env python3
"""M0 versioned Verilator metadata adapter.

Extracts, from a verilated model's frontend dump, the facts the BVI link
step needs: top-module ports (verilator name, origName, direction, width,
pinIndex), parameters, timescale, and the presence of delays or DPI.

Two formats, selected by probing the verilator binary:
  - XML  (--xml-only): present through 5.045 (distro 5.020 uses this).
  - JSON (--json-only): the replacement from 5.046 onward.
The adapter returns one normalized dict either way; nothing downstream
may look at the raw dump.
"""
import json
import re
import subprocess
import sys
from pathlib import Path


def verilator_version(vlt):
    out = subprocess.run([vlt, "--version"], capture_output=True, text=True).stdout
    m = re.search(r"Verilator (\d+)\.(\d+)", out)
    return (int(m.group(1)), int(m.group(2))) if m else (0, 0)


def pick_format(vlt):
    maj, minor = verilator_version(vlt)
    return "xml" if (maj, minor) < (5, 46) else "json"


def run_dump(vlt, top, sources, ydirs, defines, mdir, fmt, extra_args=()):
    # NOTE (M0 discovery): the inspection dump runs with --timing so
    # delay constructs SURVIVE into the AST -- under --no-timing
    # verilator discards them before dumping and no warning fires,
    # so a --no-timing dump cannot power a delay refusal.
    cmd = [vlt, "--cc", "--timing", f"--{fmt}-only",
           "--top-module", top, "-Mdir", str(mdir)]
    for d in ydirs:
        cmd += ["-y", str(d)]
    for k, v in (defines or {}).items():
        cmd += [f"-D{k}={v}" if v is not None else f"-D{k}"]
    cmd += list(extra_args)
    cmd += [str(s) for s in sources]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError(f"verilator {fmt} dump failed:\n{r.stderr}")
    return mdir


def _parse_xml(path, top):
    text = Path(path).read_text()
    widths = {}
    for m in re.finditer(r"<basicdtype [^>]*>", text):
        attrs = dict(re.findall(r'(\w+)="([^"]*)"', m.group(0)))
        if "left" in attrs:
            widths[attrs["id"]] = abs(int(attrs["left"]) - int(attrs["right"])) + 1
        else:
            widths[attrs["id"]] = 1
    # scope to the top module element
    mods = re.split(r"(?=<module )", text)
    ports, params = [], {}
    found_top = False
    for chunk in mods:
        head = re.match(r'<module [^>]*name="([^"]*)"[^>]*>', chunk)
        if not head or head.group(1) != top:
            continue
        found_top = True
        for vm in re.finditer(r"<var [^>]*>", chunk):
            attrs = dict(re.findall(r'(\w+)="([^"]*)"', vm.group(0)))
            if "dir" in attrs:
                ports.append({
                    "name": attrs["name"],
                    "orig_name": attrs.get("origName", attrs["name"]),
                    "dir": attrs["dir"],
                    "width": widths.get(attrs.get("dtype_id"), 1),
                    "pin_index": int(attrs.get("pinIndex", "0")),
                })
            elif attrs.get("param") == "true" or attrs.get("vartype") == "parameter":
                params[attrs.get("origName", attrs["name"])] = attrs.get("value")
    if not found_top:
        raise RuntimeError(f"top module {top!r} not found in XML dump")
    has_delay = "<delay" in text
    # M0 discovery: 5.020's XML carries NO DPI marker at all -- a DPI
    # import's <func> is indistinguishable from a plain function.
    # has_dpi=None means UNKNOWN; the caller must backstop by checking
    # for V<top>__Dpi.h emission after the real --cc run.
    has_dpi = None
    ts = re.search(r'timeprecision="([^"]*)"', text)
    return {"format": "xml", "ports": ports, "params": params,
            "has_delay": has_delay, "has_dpi": has_dpi,
            "timeprecision": ts.group(1) if ts else None}


def _walk_json(node, fn):
    if isinstance(node, dict):
        fn(node)
        for v in node.values():
            _walk_json(v, fn)
    elif isinstance(node, list):
        for v in node:
            _walk_json(v, fn)


def _parse_json(path, top):
    tree = json.loads(Path(path).read_text())
    # pass 1: dtype table -- nodes with an addr and a bit range
    widths = {}
    def collect_types(n):
        if isinstance(n, dict) and n.get("addr") and "DTYPE" in str(n.get("type", "")):
            rng = n.get("range")
            if rng:
                m = re.match(r"(\d+):(\d+)", str(rng))
                widths[n["addr"]] = (abs(int(m.group(1)) - int(m.group(2))) + 1) if m else 1
            else:
                widths[n["addr"]] = 1
    _walk_json(tree, collect_types)

    ports, params = [], {}
    state = {"has_delay": False, "has_dpi": False,
             "timeprecision": None, "found_top": False}

    def visit_module(mod):
        if not (isinstance(mod, dict) and mod.get("type") == "MODULE"
                and mod.get("name") == top):
            return
        state["found_top"] = True
        pin = [0]
        def visit(n):
            if not isinstance(n, dict):
                return
            t = n.get("type")
            if t == "VAR":
                direction = n.get("direction")
                if n.get("isPrimaryIO") and direction and direction != "NONE":
                    pin[0] += 1
                    ports.append({
                        "name": n.get("verilogName", n.get("name")),
                        "orig_name": n.get("origName", n.get("name")),
                        "dir": direction.lower(),
                        "width": widths.get(n.get("dtypep"), 1),
                        "pin_index": pin[0],
                    })
                elif n.get("varType") == "GPARAM":
                    params[n.get("origName", n.get("name"))] = None
            if t in ("DELAY", "DELAYSCHEDULER") or (t == "TIMINGCONTROL"):
                state["has_delay"] = True
            if isinstance(n.get("dpiImport"), bool) and n["dpiImport"]:
                state["has_dpi"] = True
            if n.get("dpiExport"):
                state["has_dpi"] = True
        _walk_json(mod, visit)

    _walk_json(tree, visit_module)
    if not state["found_top"]:
        raise RuntimeError(f"top module {top!r} not found in JSON dump")
    return {"format": "json", "ports": ports, "params": params,
            "has_delay": state["has_delay"], "has_dpi": state["has_dpi"],
            "timeprecision": state["timeprecision"]}


def extract(vlt, top, sources, ydirs=(), defines=None, workdir="obj_meta", extra_args=()):
    fmt = pick_format(vlt)
    mdir = Path(workdir)
    mdir.mkdir(parents=True, exist_ok=True)
    run_dump(vlt, top, sources, ydirs, defines, mdir, fmt, extra_args)
    if fmt == "xml":
        dumps = list(mdir.glob("*.xml"))
    else:
        dumps = [p for p in mdir.glob("*.tree.json")] or list(mdir.glob("*.json"))
    if not dumps:
        raise RuntimeError(f"no {fmt} dump produced in {mdir}")
    parse = _parse_xml if fmt == "xml" else _parse_json
    return parse(dumps[0], top)


if __name__ == "__main__":
    vlt, top = sys.argv[1], sys.argv[2]
    meta = extract(vlt, top, sys.argv[3:])
    json.dump(meta, sys.stdout, indent=2)
    print()
