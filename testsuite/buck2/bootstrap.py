#!/usr/bin/env python3
"""Download the pinned Linux Buck2 into the ignored validation directory."""

import hashlib
import json
from pathlib import Path
import platform
import subprocess
import urllib.request


def main():
    here = Path(__file__).resolve().parent
    pin = json.loads((here / "buck2-pin.json").read_text())
    if platform.system() != "Linux" or platform.machine() != "x86_64":
        raise SystemExit("this initial Buck2 pin supports Linux x86_64 only")
    directory = here.parent / ".stage1-validation" / "buck2-tool"
    directory.mkdir(parents=True, exist_ok=True)
    archive = directory / f"buck2-{pin['release']}.zst"
    if not archive.exists():
        temporary = archive.with_suffix(".download")
        try:
            with urllib.request.urlopen(pin["url"]) as response, temporary.open("wb") as output:
                while block := response.read(1024 * 1024):
                    output.write(block)
            temporary.replace(archive)
        finally:
            temporary.unlink(missing_ok=True)
    actual = hashlib.sha256(archive.read_bytes()).hexdigest()
    if actual != pin["sha256"]:
        raise SystemExit(f"Buck2 archive checksum mismatch: {archive}")
    executable = directory / "buck2"
    subprocess.run(["zstd", "-d", "-f", str(archive), "-o", str(executable)], check=True)
    executable.chmod(0o755)
    subprocess.run([str(executable), "--version"], check=True)
    print(executable)


if __name__ == "__main__":
    main()
