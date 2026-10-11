# like-messages

A Claude Code mod for "liking" agent messages.

- Every block of an agent reply gets a small `♡` under it. Press it (click, or
  ctrl+x tab to focus the transcript's controls) and it becomes `♥ liked`.
  Press again to unlike.
- `/like` toggles a like on the most recent agent reply, for when you'd rather
  not reach for the mouse.
- `/likes` opens a pane listing what you've liked, newest first, with `♥` to
  unlike.

- Agents can like messages too, via a `like` tool the plugin gives the model
  (`mcp__like-messages__like`). An agent can like your prompts, another
  agent's replies (a subagent's, the main agent's, a peer session's), and
  give a one-line reason. It can't like its own messages. The model never sees
  message ids, so it names the message by quoting a verbatim snippet; the most
  recent message containing it is liked.
  - On an agent reply, agent likes show as `✦ liked by Claude` beside your `♡`.
  - On your prompt, the engine still draws the row; `✦ liked by …` is added
    beneath it.
  - A toast says when an agent likes something, and the `/likes` pane lists
    who liked what, and why.

Likes are keyed by the message text (a hash of it, whitespace-squashed), so
identical messages share likes. They are kept in the plugin's `$.store`, so they persist across sessions.
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
