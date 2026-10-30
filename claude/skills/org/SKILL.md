---
name: org
description: Capture and edit entries in the user's org-mode files (todos, projects, tickler, inbox). Use when the user asks to capture a TODO, file something into org, update/reschedule/close an entry, or says "put this in org". Applies the change directly and commits it.
---

# Org: capture & edit

Make changes to the user's org-mode files and leave every change committed
in git, so a commit is the unit the user reviews or reverts.

Source of truth for org config is the user's Emacs init file (`user-init-file`;
keywords, tags, capture templates, logging). The facts below are distilled
from it; if something seems off, re-check init.el rather than guessing.

## File map

All files live in one org directory: the directory of the `Todo` capture
template's target in init.el. It sits inside a git repo; find the root with
`git -C <org-dir> rev-parse --show-toplevel`. `<org-dir>` below means that
directory.

| File | Role |
|---|---|
| `Inbox.org` | Capture inbox (mostly from Orgzly on phone). Unprocessed; entries are often bare `*` headings, no keyword. |
| `todos.org` | Main flat list of standalone `* TODO` entries. Default destination for new actionable items. |
| `projects.org` | `* PROJ` trees with nested `** TODO` children. Use when the item belongs to an existing project. |
| `Tickler.org` | Date-driven items: SCHEDULED/DEADLINE, often with repeaters. |
| `maybe.org` | Someday/maybe. |
| `thinking-about.org` | Open questions and musings, not tasks. |
| `reference.org`, `books-to-read.org` | What they say. |
| `archive/`, `*_archive` | Archived subtrees. Read-only for this skill. |

A `#+FILETAGS: :secondary:` file is excluded from
the main agenda views — don't put primary TODOs in one.

## Entry conventions

Keywords: `TODO(t) PROJ(p) WAIT(w@/!) | DONE(!) OBE(@)`. PROJ marks a
hierarchical project; WAIT means blocked-but-not-stuck; OBE = overcome by
events. Entering WAIT or OBE requires a logbook note explaining why.

Heading shape:

    * TODO [#B] Do the thing                                        :@personal:easy:
    :LOGBOOK:
    - Created on [2026-09-09 Tue 12:34]
    :END:

    Optional body: links, context, checklists.

- Priorities `[#A] [#B] [#C]`; no cookie means C (the default). Match the
  urgency the user expresses; omit rather than invent.
- New captures get the `- Created on [timestamp]` logbook line (this mirrors
  the user's capture templates). Generate the timestamp with
  `date +'[%Y-%m-%d %a %H:%M]'` — never write one from memory.
- All logging goes into `:LOGBOOK:` drawers (state changes, reschedules,
  refiles). When closing an item, add `CLOSED: [timestamp]` under the heading
  and a `- State "DONE" from "TODO" [timestamp]` logbook line.
- Tags are right-aligned toward column 85 by Emacs; align best-effort with
  neighboring entries, exactness doesn't matter.
- Tags come from `org-tag-alist` in init.el: exactly one context tag per
  entry from its first exclusive group (the `@`-prefixed contexts), an
  optional size tag from the second (`small`/`medium`/`large`), and any of
  the non-exclusive attribute tags that follow.
- Tag every entry explicitly, including children of already-tagged parents —
  the user tags manually and does not want to rely on tag inheritance, so
  entries stay self-contained when refiled.
- SCHEDULED/DEADLINE use active `<...>` timestamps and go on the line directly
  under the heading; repeaters like `++1m` are common in Tickler.org.

Placement: append new standalone TODOs at the end of `todos.org`; nest
project-bound TODOs as `**` children under the matching PROJ in
`projects.org`; date-driven items go to `Tickler.org`. When unsure which PROJ
(or whether one applies), pick the closest fit and name the choice in the report.

Thinking-about entries are items not yet clarified. Give each new one a
`Question: ...` body line naming what would settle it ("Worth an afternoon?
Need last year's fees."). Draft one from the context and name it in the
report; don't block the capture on asking.

Splitting: when an entry's work gets split organically (part done, part
deferred), scope the existing entry DOWN to what got finished -- retitle it
to match -- and close it. Capture the rest as a new entry that names the
one it was split from.

## Workflow

1. **Baseline check.** Run `git status` and `git diff --stat` scoped to
   `org/`. If org files are already dirty, offer to commit that pre-existing
   state first as its own commit (subject like `Org updates through
   2026-09-09`) so the user's untracked hand-edits aren't tangled with yours.
   Never mix the two in one commit.
2. **Apply.** Make the edit per Edit mechanics below. No approval round:
   the commit is the review.
3. **Commit.** First re-run `git diff` on the files about to be staged and
   check it contains only your change — the user works in the same
   files live, and an edit they save mid-flow will otherwise ride into the
   commit unnoticed. If extra changes appear, surface them and commit them
   separately. Then stage only the org files just edited (explicit paths,
   per global git rules) and commit with a descriptive subject, e.g.
   `Capture dashboard secret rotation TODO`. One logical change per
   commit; a batch of captures from one conversation can share a commit.
4. **Report.** State what was committed (hash + subject) and summarize
   what went where, including any placement judgment calls.

If the user wants something changed afterwards, make it a follow-up commit.

## Edit mechanics

Pick the mechanism by the kind of change:

- **New entries** (captures): plain text via the Edit tool, following the
  entry conventions above — this mirrors what org-capture itself produces.
- **Mutations of existing entries** (todo state, tags, priority, scheduling):
  drive org-mode itself in batch Emacs, so logging, drawers, and tag
  alignment come from the user's real config instead of imitation:

      emacs --batch -l <init-file> --eval '(progn
        (find-file "<org-dir>/todos.org")
        (goto-char (point-min))
        (search-forward "* TODO [#B] Exact heading text")
        (org-todo "DONE")
        (run-hooks (quote post-command-hook))
        (org-set-tags (list "@personal" "easy"))
        (org-schedule nil "2026-09-15")
        (save-buffer))'

  Notes: startup is ~2s, so batch many edits into one invocation. The
  "Cannot load htmlize" warning is harmless. `run-hooks post-command-hook`
  after `org-todo` flushes the deferred logbook note — without it no
  `- State ...` line is written. Known wrinkle: in a batch test the
  `CLOSED:` planning line was NOT added on DONE even though the logbook
  line was; verify (and add it manually if needed) when closing entries.

For applying, prefer the user's live Emacs session when its server is up
(init.el runs `server-start`; probe with `emacsclient -e t`): run the same
elisp via `emacsclient -e '(progn ...)'`. Editing live buffers removes any
disk-vs-buffer divergence — but first check
`(buffer-modified-p (get-file-buffer "<path>"))` and STOP if non-nil, since
`save-buffer` would silently save the user's half-finished edits along with
yours. If the server is down, apply with `emacs --batch` instead (the user
has `global-auto-revert-mode`, so their unmodified buffers pick up the
change).

Boundary, per user preference: mutations go through org-mode itself, never
hand edits imitating what org would write. When inline elisp gets awkward,
the answer is a standalone function in init.el (alongside
`org-assign-ids-to-projects`) that the user can also run interactively —
never tooling private to this skill. The user is not an Emacs/elisp expert: they review the
committed org text, not the elisp, and explanations should be in terms of the
resulting text, not Emacs internals.

## Landmines

- The directory is full of editor backups (`foo.org~`, `#foo.org#`) and may
  grow Syncthing conflict files (`*.sync-conflict-*`). Never edit, stage, or
  base an edit on any of these.
- The user works in Emacs with `global-auto-revert-mode`, so on-disk edits
  show up in their buffers — but only if the buffer is unmodified. If they
  say they have unsaved changes in a file, don't edit it; wait.
- Read the real current text immediately before editing (Syncthing
  and phone capture mean files change between sessions).
