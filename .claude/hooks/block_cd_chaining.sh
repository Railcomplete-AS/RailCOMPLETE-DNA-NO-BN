#!/bin/bash
# PreToolUse hook: block cd-chaining (cd foo && ...) and git -C usage
# so that fine-grained Bash permission rules are not bypassed.

input=$(cat)
command=$(echo "$input" | jq -r '.tool_input.command // empty')

# Strip single- and double-quoted strings to avoid false positives in message text
unquoted=$(echo "$command" | sed "s/'[^']*'//g; s/\"[^\"]*\"//g")

if echo "$unquoted" | grep -qE '\bcd\s+\S+\s*(&&|;|\|)'; then
  echo "BLOCKED: Do not chain cd with other commands. Use absolute paths or separate bash calls." >&2
  exit 2
fi

if echo "$unquoted" | grep -qE '\bgit\s+-C\b'; then
  echo "BLOCKED: Do not use git -C. Use a separate cd call first, then run git commands normally." >&2
  exit 2
fi
