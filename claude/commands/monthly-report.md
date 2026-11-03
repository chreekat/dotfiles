Generate a monthly invoicing check and work summary.

## Usage

`/monthly-report [YYYY-MM]`

If no month is given, defaults to last month.

## Steps

1. Run `monthly-report $MONTH` (the shell script at ~/bin/monthly-report) and
   capture all output. This provides:
   - invoicer check results (missing s: tags)
   - invoicer status (hours breakdown per contract)
   - time entries from Log.txt
   - submodule commits for the month
   - gap analysis (commits without nearby log entries)
   - schedule reconciliation (each day's narrative blocks vs its timedots,
     with logged hours and an umbrella-entry callout)

2. Read the relevant sections of each Log.txt for the target month to get the
   narrative context. Contract directories are listed in ~/contracts.txt (one
   name per line; each corresponds to ~/$name/).

3. Perform semantic gap analysis: beyond the date-based gap detection from the
   script, look for submodule commits that don't clearly correspond to any
   logged work category. Particularly look for:
   - Repos with commits that aren't mentioned in any time entry description
   - Days with logged time where the commit volume seems too high or too low
     for the hours recorded
   - Walk the schedule reconciliation, one day at a time, reconciling per
     *session* rather than per day -- a day is not accounted just because it
     has a timedot, nor unaccounted just because it lacks one. The [NNTN...]
     pattern shows narrative blocks (N) interleaved with timedots (T) in log
     order; read it as the schedule of the day. The flags are prompts, not
     verdicts:
     - NO-TIMEDOT (narrative, nothing logged): usually a real miss, BUT first
       check the umbrella list -- a lump entry logged on another day may cover
       it. A whole conference booked as one "events:*" line, or a week's ops
       recapped as one big entry, legitimately leaves its days blank. Textual
       distance is not temporal distance: coverage can sit days away.
     - SKEW (blocks pile up ahead of one trailing timedot, e.g. [NNNNT]): a
       day can hold several sessions; one logged does not account for the
       rest. Match each block's *topic* to a timedot -- a morning of one job
       and an afternoon of another, with only the afternoon billed, reads as
       [NNNNT] with real time missing behind the leading blocks.
     For any block that no timedot plausibly covers, judge:
     (a) Is it billable work, or just status/notes ("back on the horse",
         "lunch", "will trigger tomorrow", a dead-end you won't bill)?
     (b) If billable, what larger job/category does it join?
     (c) Is that job's time logged somewhere that plausibly covers this
         session -- possibly a lump, possibly on another day, possibly next
         month? Triggering commits often land in a different month than the
         narrative, so the commit-anchored gap analysis misses these; the
         schedule reconciliation is what catches them.

4. Check org files in ~/Syncthing/PhoneFiles/org/ for each contract. These
   standard org-mode files use tags to associate entries with contracts. For
   each contract name (from ~/contracts.txt), search for its tag across all
   .org files. Relevant
   files include but are not limited to: todos.org, projects.org, maybe.org,
   Tickler.org, thinking-about.org, proposals.org, and their archive files.
   Collect:
   - Open TODOs and active PROJs tagged for this contract
   - Items completed (DONE) during the target month
   - Scheduled/tickler items that fell within the target month
   - Deferred ideas and project proposals (especially in maybe.org)

5. Identify ongoing work and next actions by combining the Log.txt narrative
   (step 2) with the org file context (step 4). Look for:
   - Work that was started but not finished (look for TODOs, "continue next
     week", open PRs, WIP branches in the log narrative)
   - Threads that were deferred or explicitly postponed
   - Natural follow-ups to completed work
   - Read the LAST few entries in each Log.txt (the most recent week) to see
     what's currently active heading into the new month
   - Open org TODOs/PROJs that relate to work done this month
   - Tickler items coming due in the next month

6. Build a waiting list, as of today (not the target month):
   - Org WAIT entries tagged for each contract, with the reason from their
     latest logbook note.
   - My open PRs: `gh search prs --author=@me --state=open --json
     repository,number,title,url,isDraft --limit 100`, then for each
     `gh pr view <n> --repo <owner/repo> --json
     reviewDecision,mergeable,statusCheckRollup` (search results lack review
     state). Assign each PR to a contract by its repository owner, checking the
     Log.txt narrative when the owner is ambiguous; list unassignable PRs
     separately rather than guessing.
   Sort everything into two buckets:
   - **Waiting on others**: WAIT entries, and PRs with no approving review
     yet (`REVIEW_REQUIRED`, or empty review state with no reviewer).
   - **Waiting on me**: PRs that are approved (ready to merge -- note any
     failing checks or merge conflicts), PRs with changes requested, drafts,
     and WAIT entries whose blocker the log or PR state shows is now on my
     side.

7. Present the report organized **by contract**. For each contract, include:
   - **Issues**: Any invoicer check failures or missing tags that need fixing
   - **Gap analysis**: Both date-based gaps (from the script) and semantic gaps
     (commits that might represent unlogged work)
   - **Monthly summary**: A narrative outline suitable for writing a public
     monthly update post. Group work into themes (completed features,
     incidents, infrastructure, ongoing projects). Include hours per theme.
   - **Ongoing work & next actions**: What's in progress and what the logical
     next steps are, drawing from both Log.txt and org files. Distinguish
     between:
     - Active (explicitly in progress at month end)
     - Deferred (mentioned but postponed)
     - Recommended (natural follow-ups based on the work done)
     - Org backlog: open TODOs/PROJs from org files relevant to this month's
       work, and any tickler items coming due next month
   - **Waiting**: the two buckets from step 6, each item with its link and a
     one-line status (e.g. "approved, checks green -- merge it").

## Notes

- The gap analysis is meant to be a conversation starter, not a definitive
  list. Some "gaps" are expected (e.g. rebased commits, squashed work).
- The monthly summary should be high-level and focus on outcomes, not
  individual commits.
- If there are invoicer check failures, offer to fix them.
- Org files live in ~/Syncthing/PhoneFiles/org/. They use standard org-mode
  conventions (TODO/DONE/PROJ/WAIT states, SCHEDULED, CLOSED, :LOGBOOK:
  entries). Contract tags are snake_case versions of the contract directory
  name. Search all .org files (not just the obvious ones -- items
  get refiled between files). Archive files (*.org_archive) contain completed
  items and may be useful for finding work done during the target month.
