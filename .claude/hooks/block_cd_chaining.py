import json
import re
import sys

input_data = json.load(sys.stdin)
command = input_data.get("tool_input", {}).get("command", "")

# Strip quoted strings so we only match actual command structure, not message text
unquoted = re.sub(r'"[^"]*"', '""', re.sub(r"'[^']*'", "''", command))

# Block cd/pushd chained with other commands via &&, ;, |, &, or newline
if re.search(r'\b(cd|pushd)\s+\S+\s*(&&|;|\|\|?|&|\n)', unquoted):
    print("BLOCKED: Do not chain cd/pushd with other commands. Use absolute paths or separate bash calls.", file=sys.stderr)
    sys.exit(2)

# Block git -C anywhere in the argument list
if re.search(r'\bgit\s+.*-C\b', unquoted):
    print("BLOCKED: Do not use git -C. Use a separate cd call first, then run git commands normally.", file=sys.stderr)
    sys.exit(2)
