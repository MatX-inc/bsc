"""Extract the model's final JSON object from a claude-code-action execution file.

execution_file is a JSON array of SDK messages; the final {"type":"result"} message's `result`
field holds the model's final text. Pull the JSON object out of it (tolerant of fences/prose).
Prints JSON to stdout; nonzero + diagnostics to stderr on failure."""

import json
import sys


def final_text(path):
    with open(path, encoding="utf-8") as fh:
        data = json.load(fh)
    seq = data if isinstance(data, list) else [data]
    for m in seq:
        if (
            isinstance(m, dict)
            and m.get("type") == "result"
            and isinstance(m.get("result"), str)
        ):
            last = m["result"]
    last = locals().get("last", "")
    if last:
        return last, seq
    for m in reversed(seq):
        if isinstance(m, dict) and m.get("type") == "assistant":
            content = (m.get("message") or {}).get("content") or m.get("content")
            if isinstance(content, str):
                return content, seq
            if isinstance(content, list):
                for blk in content:
                    if (
                        isinstance(blk, dict)
                        and blk.get("type") == "text"
                        and blk.get("text")
                    ):
                        return blk["text"], seq
    return "", seq


def extract_json(text):
    dec = json.JSONDecoder()
    i = text.find("{")
    while i >= 0:
        try:
            _, end = dec.raw_decode(text, i)
            return text[i:end]
        except ValueError:
            i = text.find("{", i + 1)
    return None


def main(argv):
    if len(argv) < 2:
        sys.stderr.write("usage: extract_structured.py <execution_file>\n")
        return 1
    try:
        text, seq = final_text(argv[1])
    except (OSError, ValueError) as e:
        sys.stderr.write("could not read execution file: %s\n" % e)
        return 1
    obj = extract_json(text)
    if obj is None:
        sys.stderr.write("no JSON object found in model output\n")
        types = [m.get("type") for m in seq if isinstance(m, dict)]
        sys.stderr.write("DIAG message types (last 15): %r\n" % types[-15:])
        sys.stderr.write("DIAG final_text len=%d head=%r\n" % (len(text), text[:1800]))
        return 1
    sys.stdout.write(obj)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
