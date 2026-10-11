# like-messages

A Claude Code mod for "liking" agent messages.

- Every block of an agent reply gets a small `♡` under it. Press it (click, or
  ctrl+x tab to focus the transcript's controls) and it becomes `♥ liked`.
  Press again to unlike.
- `/like` toggles a like on the most recent agent reply, for when you'd rather
  not reach for the mouse.
- `/likes` opens a pane listing what you've liked, newest first, with `♥` to
  unlike.

Likes are kept in the plugin's `$.store`, so they persist across sessions.
Each like records the message text (first 4000 chars) and when you liked it.

## Install

```
/plugin install like-messages --marketplace chreekat/dotfiles
```

Answer `y` to add the marketplace, then pick a scope (user is the default).

## Develop

```
claude --plugin-dir ~/dotfiles/claude/plugins/like-messages
claude plugin validate ~/dotfiles/claude/plugins/like-messages
claude plugin test ~/dotfiles/claude/plugins/like-messages
```

## Caveat

To put the heart under a message, the mod draws the message itself (as
Markdown, with its own `⏺` bullet on the terminal) instead of letting the
engine draw it, so its look can differ slightly from an unhooked reply.
Summary blocks are left to the engine.
