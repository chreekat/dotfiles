# Rules

Coding rules:
- Use TDD: write a failing test before implementing behavior.
- Commit early and often. Write one logical change per commit.
- Run tests before declaring a task complete.
- The final tests for a task, including integration tests, SHOULD be scripted and
  included in the repo.
- Ask before introducing new dependencies.
- Prefer understanding existing patterns over inventing new ones. Do not reinvent the wheel.
- Comments explain BEHAVIOR, commit messages explain CHANGE. Only add comments
  about changes if it's absolutely critical to understanding the code on its
  own.

Haskell rules:
- Always write shrinks for Arbitrary instances. If the type does not admit a
  good shrink, suggest how it could be modified to enable it.
- Do not improvise on version bounds. Use existing bounds if found in the
  project, otherwise ask me what they should be.
- Do not use show or read for serializing or deserializing. They are only for
  debugging.
- Never pipe `cabal build` to 'head'. Cabal does not handle SIGPIPE properly.
  You CAN pipe it to 'tail', however.
- Use four-space indent.
- Use diff-minimizing whitespace.

Planning rules:
- A plan should be broken down into committable steps. A rule of thumb seems to be
  that a single step takes 1-8 commits.
- For prompts requiring larger plans -- say, more than 5 steps -- it should
  first be broken into larger, demo-able milestones of 1-5 steps. Any larger
  than that and we need to have a discussion.
- Think carefully about when a prototype or a tracer bullet might be a better
  strategy for developing new features.
- Use BDD: organize new development into observable, demoable behaviors.
- Functions are the fundamental unit of organization.
- Types and database schemas are secondary artifacts to functions. Only develop
  them on demand. Start with small types and grow them as a function is fleshed
  out.
