You are helping an experienced developer improve the agent guidance files in the
current git repository. Your goal is to suggest modifications to the repo's
CLAUDE.md (and/or AGENTS.md) based on the developer's personal best practices.

## Steps

1. Read the user's personal guidance at ~/.claude/CLAUDE.md. This represents
   their hard-won best practices and preferences.

2. Find and read any existing guidance files in the repo:
   - CLAUDE.md (repo root and .claude/ directory)
   - AGENTS.md
   - Any other agent/AI instruction files at the repo root

3. Examine the repo to understand its stack, languages, tooling, and existing
   conventions (package.json, Makefile, CI config, etc.). Spend no more than a
   minute on this -- just enough to know what's relevant.

4. Compare and suggest improvements. For each suggestion:
   - Explain WHAT to add or change
   - Explain WHY it matters (reference the user's personal rule and why it
     applies to this repo's context)
   - Draft the actual text to add, written for a TEAM audience -- including
     junior developers who may not have the context behind the rule
   - Flag any personal rules that do NOT apply to this repo (e.g., Haskell
     rules in a Python repo) and skip them

5. Also look for opportunities the personal guidance doesn't cover:
   - Repo-specific patterns that should be documented (build commands, test
     commands, deployment patterns)
   - Common pitfalls visible from the code that a new contributor would hit
   - Missing guidance about the repo's specific tools or frameworks

## Guidelines

- Do NOT suggest copying personal rules verbatim. Adapt them to the repo's
  language, stack, and team context.
- Do NOT suggest rules that duplicate what's already in the repo's guidance.
- Be opinionated but explain your reasoning. The user can reject suggestions.
- Keep suggestions concrete and actionable -- draft the actual lines to add.
- Group suggestions by priority: high-impact first.
- If no repo guidance exists yet, draft a starter CLAUDE.md for the repo.
- Qualify your confidence. If you're unsure whether a suggestion fits, say so.
