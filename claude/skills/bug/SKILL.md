---
name: bug
description: Pick a few easy, independent bugs from the `b` tracker and fix them in parallel subagents running in git worktrees. Use when the user says "let's solve bugs", "/bug", "knock out some bugs", or asks to work through the `b` backlog.
---

# Bug: parallel backlog fixing

Fix bugs from the current repo's `b` tracker by fanning out to subagents in
isolated git worktrees. Default cadence: **pick 3 easy, independent bugs and
let them rip.**

## The `b` utility

`b` is a thin wrapper around `t` (losh-t) operating on the `bugs` list stored at
`$(git rev-parse --git-common-dir)/bugs`, so it is per-repo. Anything after `b`
is passed straight to `t`.

| Command | What it does |
|---|---|
| `b` | List open bugs with short ids (e.g. `0`, `4`, `bc01`) |
| `b -v` | List open bugs with **full** 40-char ids |
| `b -i ID` | **Inspect** one bug (open or done) by id prefix |
| `b --done` | List finished bugs |
| `b -g WORD` | List only bugs whose text contains WORD |
| `b -f ID` | Mark bug ID **finished** |
| `b -r ID` | **Remove** bug ID |
| `b -e ID TEXT` | Edit bug ID to contain TEXT |

**CRITICAL FOOTGUN:** With no action flag, `t` treats trailing TEXT as *add a
new task*. So `b 7` does **not** show bug 7 — it **creates a junk bug titled
"7"**. Never run `b <id>` to inspect a bug.
- To read one bug, use `b -i <id>` (accepts a short or full id prefix; searches
  open **and** done). Requires the wrapper from the user's NixOS config — if
  `b -i` errors as an unknown option, that host hasn't been `nixos-rebuild`ed
  yet; fall back to `b -v` / `b -g <keyword>`.
- If you accidentally create junk bugs, remove them with `b -r <full-id>`
  (get the id from `b -v`).

Short ids shown by `b` are unique prefixes of the full id; `-f`/`-r`/`-e`
accept either the short id or the full id.

## Workflow

1. **Survey.** Run `b` to list open bugs. Read each candidate's text.
2. **Pick 3 (unless the user says otherwise).** Choose bugs that are:
   - well-scoped (a clear, verifiable fix — not open-ended research), and
   - **independent** — touching *different* areas of the codebase so worktrees
     don't collide (e.g. one persist/restore, one rendering, one concurrency
     path). Avoid picking two bugs in the same subsystem.
   State which 3 you picked and why before spawning.
3. **Spawn one subagent per bug, each in its own git worktree**, in parallel
   (all Agent calls in a single message). Give each agent:
   - the full bug text and its `b` id,
   - `isolation: "worktree"` so it works on an isolated checkout,
   - the standing instructions below (which include re-basing onto local
     `main` — worktrees are cut from `origin/main`, which may lag local work).
4. **Integrate.** When agents finish, cherry-pick each fix onto the main branch
   (they commit but do **not** push). Resolve any conflicts.
5. **Verify the combined result.** From the main checkout, build and run the
   full test suite using the project's own tooling. Expect a clean build (zero
   warnings) and a fully green suite. Treat any failure or slowdown as a
   drop-everything issue.
6. **Close.** For each integrated fix, `b -f <id>`.
7. **Deploy** only after the user's usual gate (tests green + committed), using
   whatever deploy step that project defines (check project memories/docs).

## Standing instructions for each bug subagent

- **Base on local `main` first.** Your worktree is cut from `origin/main`, which
  can lag the local `main` branch (git is distributed — the remote is not the
  source of truth here). Before writing any code, run `git reset --hard main` to
  move your worktree onto the current local `main` tip. The worktree shares the
  repo's object store, so `main` resolves even though your branch was cut from
  the remote. This makes your fix build on the latest local work and cherry-pick
  cleanly.
- **TDD:** write a failing test that specifies the fix *first*, then implement.
  A new feature's failing test IS its behavior spec; a bug fix gets a
  bug-capture test that fails on the old code and passes on the fix.
- Follow all repo/global rules for this project: match its formatting and
  conventions, keep the build warning-free ("pre-existing" is not an excuse),
  use diff-minimizing whitespace, and drive everything through the project's
  standard build/test commands.
- Keep the whole suite fast; never synchronize a test with a fixed sleep.
- **Commit early and often**, one logical change per commit, subject-line-only
  messages (no body unless the reason is non-obvious), **no AI/agent trailers**.
- **Do not push.** Leave commits on the worktree branch for integration.
- Report back: the bug id, a one-line summary of the fix, files touched, and
  confirmation the suite is green.

## Scaling

3 is the default, not a rule. If the user names a count or specific bugs, honor
that. Audit/report-style bugs (ones asking for analysis rather than a fix)
parallelize well too, and their output is a report rather than a diff.
