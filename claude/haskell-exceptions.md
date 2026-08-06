# Haskell exceptions & structured concurrency

One distinction governs everything: **synchronous** exceptions (raised by the
code you called — `IOException`, `SQLError`, a pattern-match failure) vs
**asynchronous** ones (thrown *to* your thread from elsewhere — `ThreadKilled`,
`AsyncCancelled`, a `timeout` firing). You handle the synchronous ones; you
almost never *catch* the asynchronous ones — let them through and rely on
`bracket` to clean up.

## Catching

- **Never broad-catch to recover.** `catch`/`handle`/`try` on `SomeException`
  swallows async exceptions too, which silently breaks `cancel`, `timeout`, and
  clean shutdown. Catch the *specific synchronous types you actually expect*;
  they are never async, so nothing is swallowed and the handler documents what
  you anticipated.
- **Handle the expected, propagate the rest.** An expected failure (a closed fd,
  a lost lock race) → caught specifically, handled non-fatally (log +
  continue/retry). An unexpected exception is a bug → let it surface loudly,
  don't bury it.
- **A broad catch *inside a loop* is the trap.** It can eat the async exception
  that was trying to stop that loop's thread, so `cancel`/`withAsync` teardown
  hangs forever. If a loop must guard its body, guard a *specific sync type*.
- **Don't reach for `safe-exceptions` reflexively.** It encodes the sync/async
  split correctly and is fine to use, but reasoning about categories — catch
  specific sync types, never broad-catch — gets the same safety with no
  dependency. (Adding it is a version-bound decision → ask.)

## Cleanup & lifetimes — always bracket

- **Every resource with a lifetime is acquired via `bracket`/`finally`** — fds,
  handles, sockets, memory, locks. A release line a later exception can skip is
  a latent bug; make release structural.
- **Threads too: `withAsync`, not `forkIO` + `killThread`.** The bracket cancels
  the thread on every exit path (normal, exception, cancellation) and cannot
  leak it.

## Supervising background work

- **`link` is the tool for "run this long-lived thread alongside me and tell me
  if it dies."** It ignores normal completion and propagates only exceptions —
  exactly "supervise until fault."
- **`race`/`concurrently`/`Concurrently` are for *symmetric peers*** —
  first-to-finish, or run-all-and-combine. Don't force an asymmetric "one
  authoritative main + supervised daemons" shape into them: a daemon that can
  return normally will "win" a `race` and wrongly tear the group down, and
  wait-for-all will hang on an immortal daemon. When the parties aren't peers,
  keep the asymmetry honest (`withAsync body` + `link daemon`).
- **`ExitCode` is an exception with teeth.** `exitWith`/`exitSuccess` throw it;
  a broad handler swallows it (the process then won't exit), and it only exits
  the process when it reaches the *main* thread. Using it as cross-thread control
  flow is a footgun, not a shortcut.
