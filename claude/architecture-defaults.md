# Architecture defaults

Hard defaults for how new code should be shaped. These extend the rules in
[CLAUDE.md](CLAUDE.md); they are not project-specific.

## Function-argument architecture

- Pass dependencies — database connections, HTTP clients, config records,
  runtime handles — as ordinary function arguments. A concrete connection
  parameter is shorter and clearer than a typeclass-constraint salad threaded
  through a `MonadReader` stack.
- Do not introduce a typeclass to abstract over an interface that has one
  production implementation. "I want to mock this in tests" is not a reason to
  add a class; it is a reason to find the pure logic inside and test that
  directly. Records-of-functions used as a test seam are typeclass
  dictionaries with different syntax — same problem.
- Keep monad stacks small. Most code should run in `IO`, or a thin reader over a
  concrete runtime record. Functions that don't need a stack shouldn't have one
  threaded through.
- Keep business logic in pure functions or straightforward `IO`. Keep HTTP
  and runtime layers thin.
- Only add parameters that are used today. This applies to everything
  function-shaped: function arguments, record fields, type variables,
  CloudFormation/infra template parameters, URL query parameters, CLI flags,
  configuration keys. If a value is constant, inline it. If it varies in only
  one known way, accept only that. Do not add a parameter "in case someone
  needs it later."

## Layering

- Leaf I/O functions do one thing: perform the call and return the result.
  Decoding, logging, retries, and eviction-on-error belong in the calling
  shell where the trace envelope and the application monad live.
- Default to "parse, don't validate" at the boundary, and put policy choices
  (eviction, retry, fallback) where the business logic for that domain
  lives — usually not the leaf.

## Testing

- Test pure business logic — decision functions, serialization roundtrips,
  state transitions, validation helpers — with unit and property tests. If a
  piece of code is mostly orchestration over I/O, extract whatever pure
  logic it has into a named function and test that.
- Do not introduce a test-only typeclass instance, a `Mock` newtype, or a
  record-of-functions for the sole purpose of making integration-shaped code
  "testable." A test that exercises a substituted stub is not testing the
  production behavior, so it's useless.
- Better no test than a fake test. A test that constructs an in-memory
  substitute for a real dependency and then doesn't actually assert anything
  about that dependency is misleading scaffolding. Delete it.

## Logging

- Emit wide, structured events — one log line per unit of work, not a trail
  of breadcrumbs. Include all relevant context in that single line.
- Use structured JSON, not prose.
  `{"token_id":42,"action":"refresh","result":"ok","duration_ms":230}` beats
  `"Refreshing token 42… done in 230ms"` — log search tools parse JSON
  fields automatically.
- Keep logging out of pure/domain functions; return structured results and
  let the integration/boundary layer decide what to log.
- Include high-cardinality identifiers (request id, user id, entity id) so a
  single query can isolate one path through the system.
- Do not log secrets (tokens, passwords, keys). If you need to identify a
  token, log its database id, not its value.
- Reference: <https://loggingsucks.com/>

## Database access

- Each call from application code should be one round trip. If you find
  yourself writing `select; case existing of … insert …; case rowsUpdated of
  0 → insert; _ → pure …`, you have lifted SQL control flow into application
  code and paid round-trip cost for every branch. That sequence is a single
  `INSERT … ON CONFLICT` (or, with conditional side effects, a single CTE).
  Express it in SQL, not as glued-together ORM calls. The same applies to
  "back up the previous row before overwriting" patterns: one CTE-chained
  statement, not a sequence of round-trip-bound steps.
- Only use the ORM for ORM-shaped queries: single-table CRUD, simple joins. Drop
  to raw SQL for UPSERTs, CTEs, window functions, `INSERT … SELECT …`, or
  anything that wants explicit text for review.
- Wrap multi-statement work in an explicit transaction block only when one
  statement genuinely is not enough. The function signature should make the
  transaction boundary visible to a reader.

## Scheduled work

- In-process scheduling — a hand-rolled `forever $ do … threadDelay …` loop,
  or an in-process cron typeclass — is reimplementing systemd... badly.
  Schedule one-shot executables out-of-process (EventBridge Scheduler → ECS
  RunTask, systemd-timer, k8s CronJob, etc.). Cadence, jitter, and on-failure
  backoff live in the scheduler, not in the process.
- A one-shot job is a plain `main` that does the unit of work once and
  exits. Pass inputs (database pool, current time, batch size) as ordinary
  arguments to a library function.
- Pass "now" (and any other ambient state) into the library function as a
  plain argument rather than reading it inside the loop. A single sweep
  should classify every record against the same reference time, and the
  pure inputs make the decision logic easy to drive from tests.
- Exit non-zero only on systemic failure (database unreachable, every
  attempt failed). A quiet sweep with nothing to do still exits zero, so the
  schedule's failure metrics mean something.

## Haskell specifics

- Document the purpose of modules, functions, and function arguments with
  Haddock comments. Don't refer to arguments by their names in the code,
  since the code names aren't exposed in Haddocks.
- Prefer plain records and newtypes with plain field names. Use
  `OverloadedRecordDot` for new field access when it keeps code clearer.
- Favor readable term-level code over type families or associated-type
  machinery.
- If using a DB library, avoid `Arrows`, `proc`, and `SelectArr`-typed query
  helpers — they are a second syntax for what `do`-notation already expresses.
  If a query feels awkward in `do`-notation, the right escape hatch is raw SQL,
  not `proc`.
