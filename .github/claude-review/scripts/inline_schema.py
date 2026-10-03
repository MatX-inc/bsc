"""Emit a JSON schema as one inline, shell-quoted `--json-schema` token.

The Claude Code CLI's `--json-schema` takes JSON text, not a path, so the schema is compacted
and wrapped as a double-quoted token (internal quotes escaped) suitable for claude-code-action's
quote-aware `claude_args` parser. Written to $GITHUB_OUTPUT under the given key (or stdout when
run locally). The schema file stays the single source of truth; the workflow interpolates this
token after `--json-schema`.

Usage:
    inline_schema.py [schema_basename] [out_key]
      schema_basename  schema file under .github/claude-review/  (default output-schema.json)
      out_key          $GITHUB_OUTPUT key to write              (default schema_arg)
"""

import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))


def main(argv):
    basename = argv[1] if len(argv) > 1 else "output-schema.json"
    out_key = argv[2] if len(argv) > 2 else "schema_arg"
    schema_path = os.path.normpath(os.path.join(HERE, "..", basename))
    with open(schema_path, "r", encoding="utf-8") as fh:
        compact = json.dumps(json.load(fh), separators=(",", ":"))
    # Wrap as a double-quoted shell token for the action's arg parser, escaping the four
    # characters special inside double quotes (\, ", $, `) so any schema text is safe.
    for ch in ("\\", '"', "$", "`"):
        compact = compact.replace(ch, "\\" + ch)
    token = '"' + compact + '"'
    block = "%s<<__SCHEMA_EOF__\n%s\n__SCHEMA_EOF__\n" % (out_key, token)
    out = os.environ.get("GITHUB_OUTPUT")
    if out:
        with open(out, "a", encoding="utf-8") as fh:
            fh.write(block)
    else:
        sys.stdout.write(block)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
