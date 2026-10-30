---
name: orgzly-inbox
description: Resolve an Orgzly sync conflict on the org inbox (Inbox.org). Use when the user says Orgzly reports a conflict, their inbox is "fucked"/out of sync between phone and desktop, or asks to fix/unfuck/merge the inbox.
---

# Orgzly inbox conflict

Orgzly writes a new capture into its cached copy of a notebook before checking
whether the file on disk has changed. When the desktop has also changed
`Inbox.org`, Orgzly sees both sides modified and refuses to sync. Syncthing
itself is fine; the conflict lives only inside Orgzly.

The fix: move the desktop file out of the way, have Orgzly force-push its copy,
merge the two by hand, and let the next sync settle. Git in the org files'
repo holds the desktop side throughout, so nothing can be lost.

Paths: `<org-dir>` and its git repo, found as the org skill describes; run git
from the repo. Follow the org skill's landmines (never touch `*~`, `#*#`, or
`*.sync-conflict-*` files).

## Steps

1. **Baseline.** Check `git status --short <org-dir>` and list any
   `*.sync-conflict-*` files. If the Emacs server is up (`emacsclient -e t`),
   check `(buffer-modified-p (get-file-buffer "<org-dir>/Inbox.org"))`; if
   non-nil, stop and ask the user to save or discard first. If `Inbox.org` is
   dirty in git, commit it alone as `Org updates through <date>`, so the
   desktop side is recorded.

2. **Move aside.** `mv <org-dir>/Inbox.org <org-dir>/Inbox2.org`.

3. **Hand off.** Ask with AskUserQuestion, so the session shows as waiting:
   the user lets Syncthing carry the move to the phone, force-pushes
   `Inbox.org` from Orgzly, and waits for it to sync back to the desktop.

4. **Diff.** Confirm `<org-dir>/Inbox.org` exists again, then
   `diff -u <org-dir>/Inbox2.org <org-dir>/Inbox.org` (desktop vs phone). Ignore Orgzly's
   formatting churn (blank lines between entries, tag alignment).

5. **Merge.** The inbox is nearly write-only, so the merge is a union with the
   desktop as base:
   - Entries only in the phone's copy are either new phone captures (keep,
     appended at the end) or entries the desktop already refiled or deleted
     (drop). Tell them apart with git: `git log -S'<heading text>' --oneline --
     <org-dir>/Inbox.org`. If the heading never appeared in history, it is a new
     capture.
   - Entries that differ on both sides (state, notes, body): the desktop
     version usually wins, since Orgzly's copy is stale. If the phone side has
     something the desktop lacks (a note, a state change made on the phone),
     show both and ask.
   - Entries only on the desktop stay.

6. **Apply and commit.** Write the merged file to `<org-dir>/Inbox.org`, delete
   `<org-dir>/Inbox2.org`, show `git diff <org-dir>/Inbox.org` (against the baseline it
   should contain only the phone's new captures, plus anything settled in
   step 5), and commit it alone as `Merge Orgzly inbox captures`. If the Emacs
   server is up, revert the `Inbox.org` buffer so it doesn't hold the moved
   file.

7. **Report.** Commit hash, which captures came from the phone, anything
   dropped and why. The next Syncthing pull plus Orgzly sync should then load
   the merged file cleanly, because Orgzly's copy is unchanged since its
   force-push.
