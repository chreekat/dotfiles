import { atom, read, update } from 'claude-code'
import type { EngineInterface, Register } from 'claude-code'

import type { Like, Seen } from '../types'

const PANE = 'likes'
const STORE_KEY = 'likes.v2'
const OLD_STORE_KEY = 'likes'
const MAX_TEXT = 4000
const MAX_SEEN = 500
const YOU = 'you'
const MAIN_NAME = 'Claude'
const TOOL = 'like'
const TOOL_FULL = 'mcp__like-messages__like'

const likes = atom({ plugin: 'like-messages', key: 'likes' } as const, {})
const seen = atom({ plugin: 'like-messages', key: 'seen' } as const, [])
const lastReply = atom({ plugin: 'like-messages', key: 'lastReply' } as const, null)

const squash = (text: string) => text.replace(/\s+/g, ' ').trim()

// Likes are keyed by content, not by transcript id: the model never sees row
// ids, and the drawing sees the block's text, so text is what both share.
function keyOf(text: string) {
  const s = squash(text)
  let h = 0x811c9dc5
  for (let i = 0; i < s.length; i++) {
    h ^= s.charCodeAt(i)
    h = Math.imul(h, 0x01000193)
  }
  return `${(h >>> 0).toString(36)}.${s.length}`
}

const firstLine = (text: string) => {
  const line = text.split('\n').find(l => l.trim() !== '') ?? ''
  return line.length > 100 ? `${line.slice(0, 99)}…` : line
}

const agentLikers = (like: Like | undefined) =>
  like ? Object.keys(like.by).filter(who => who !== YOU) : []

// Write-through: $.state drives the drawing, $.store keeps it past the session.
async function setLike(
  $: EngineInterface,
  target: Omit<Like, 'by' | 'key'>,
  who: string,
  isOn: boolean | 'toggle',
  reason?: string,
) {
  const key = keyOf(target.text)
  const at = await $.clock.now()
  let isLiked = false
  await update($, likes, all => {
    const next = { ...all }
    const like = next[key] ?? { ...target, key, text: target.text.slice(0, MAX_TEXT), by: {} }
    const by = { ...like.by }
    isLiked = isOn === 'toggle' ? by[who] === undefined : isOn
    if (isLiked) {
      by[who] = reason ? { at, reason } : { at }
    } else {
      delete by[who]
    }
    if (Object.keys(by).length === 0) {
      delete next[key]
    } else {
      next[key] = { ...like, by }
    }
    return next
  })
  await $.store.set(STORE_KEY, await read($, likes))
  return isLiked
}

async function loadStored($: EngineInterface) {
  const stored = (await $.store.get(STORE_KEY)) as Record<string, Like> | undefined
  if (stored) {
    return stored
  }
  // v0.1 kept { id, text, likedAt } liked by the person alone.
  const old = (await $.store.get(OLD_STORE_KEY)) as
    | Record<string, { text: string; likedAt: number }>
    | undefined
  const migrated: Record<string, Like> = {}
  for (const { text, likedAt } of Object.values(old ?? {})) {
    const key = keyOf(text)
    migrated[key] = { key, text, author: 'agent', authorName: MAIN_NAME, by: { [YOU]: { at: likedAt } } }
  }
  return migrated
}

async function loopName($: EngineInterface, agentId: string | undefined) {
  if (agentId === undefined) {
    return MAIN_NAME
  }
  const agent = await $.agent.list().then(
    list => list.find(a => a.id === agentId),
    () => undefined,
  )
  return agent ? `${agent.type} (${agent.description})` : `agent ${agentId.slice(0, 6)}`
}

const LIKE_DESCRIPTION = `Like (or unlike) a message in this conversation: one the user wrote, one another agent wrote, or one a subagent or peer session sent. The user sees your like beside the message, with your reason.

Use it sparingly and sincerely, the way a person would: for a message that was genuinely helpful, insightful, kind or funny. You cannot like your own messages.

Identify the message by quoting a short verbatim snippet of it (a phrase of 3-15 words is best); the most recent matching message is liked.`

export const register: Register = on => {
  on('session.start', async ($, e, next) => {
    const stored = await loadStored($)
    await update($, likes, all => ({ ...stored, ...all }))
    await $.command.register({ name: 'like', description: 'Like (or unlike) the last agent reply' })
    await $.command.register({ name: 'likes', description: 'Show liked messages' })
    await $.tool.register({
      name: TOOL,
      description: LIKE_DESCRIPTION,
      isDeferred: false,
      inputSchema: {
        type: 'object',
        properties: {
          quote: { type: 'string', description: 'A short verbatim snippet of the message to like.' },
          author: {
            type: 'string',
            enum: ['user', 'agent'],
            description: 'Who wrote it, to disambiguate; optional.',
          },
          reason: { type: 'string', description: 'One short line on why; shown to the user.' },
          unlike: { type: 'boolean', description: 'Take a like back instead.' },
        },
        required: ['quote'],
      },
    })
    return next(e)
  })

  // Remember who said what, so an agent's quote can be traced to a message.
  on('session.append', async ($, e, next) => {
    const stored = await next(e)
    const { message, origin } = e
    const texts = message.content.flatMap(b =>
      b.type === 'text' && typeof b.text === 'string' && b.text.trim() !== '' ? [b.text] : [],
    )
    if (texts.length === 0 || message.isMeta) {
      return stored
    }
    let row: Seen | undefined
    if (message.type === 'assistant' && origin.kind === 'model') {
      row = { author: 'agent', loop: e.agentId ?? 'main', authorName: await loopName($, e.agentId), texts }
    } else if (message.type === 'user' && ['composer', 'bridge', 'sdk'].includes(origin.kind)) {
      row = { author: 'user', loop: e.agentId ?? 'main', authorName: YOU, texts }
    } else if (message.type === 'user' && (origin.kind === 'peer' || origin.kind === 'peer-send-message')) {
      row = { author: 'agent', loop: 'peer', authorName: 'another Claude session', texts }
    }
    if (row) {
      const add = row
      await update($, seen, list => [...list, add].slice(-MAX_SEEN))
    }
    return stored
  }).catch(($, e, next) => next(e))

  on('tool.call', { tool: TOOL_FULL }, async ($, e) => {
    const input = e as unknown as { quote?: unknown; author?: unknown; reason?: unknown; unlike?: unknown }
    const quote = typeof input.quote === 'string' ? squash(input.quote) : ''
    if (quote === '') {
      return { result: 'Give a short verbatim quote of the message to like.' }
    }
    const author = input.author === 'user' || input.author === 'agent' ? input.author : undefined
    const reason = typeof input.reason === 'string' ? input.reason.slice(0, 200) : undefined
    const myLoop = e.agentId ?? 'main'
    const me = await loopName($, e.agentId)

    const rows = await read($, seen)
    let found: { row: Seen; text: string } | undefined
    for (let i = rows.length - 1; i >= 0 && !found; i--) {
      const row = rows[i]
      if (!row || (author && row.author !== author)) continue
      const text = row.texts.find(t => squash(t).includes(quote))
      if (text !== undefined) found = { row, text }
    }
    if (!found) {
      return { result: `No message in this conversation contains "${quote}". Quote it verbatim.` }
    }
    const { row, text } = found
    if (row.author === 'agent' && row.loop === myLoop) {
      return { result: 'That message is your own; you cannot like your own messages.' }
    }
    const isUnlike = input.unlike === true
    await setLike(
      $,
      { text, author: row.author, ...(row.author === 'agent' ? { authorName: row.authorName } : {}) },
      me,
      !isUnlike,
      reason,
    )
    const whose = row.author === 'user' ? "the user's" : `${row.authorName}'s`
    $.ui.toast(`${isUnlike ? '♡' : '♥'} ${me} ${isUnlike ? 'unliked' : 'liked'} ${whose} message: ${firstLine(text)}`)
    return { result: `${isUnlike ? 'Unliked' : 'Liked'} ${whose} message: "${firstLine(text)}"` }
  }).catch(() => ({ result: 'Liking failed inside the like-messages plugin.' }))

  on('turn.complete', async ($, e, next) => {
    if (e.agentId === undefined && e.answer !== '') {
      await update($, lastReply, () => ({ turnId: e.turnId, text: e.answer }))
    }
    return next(e)
  })

  on('command.run', { command: 'like' }, async $ => {
    const reply = await read($, lastReply)
    if (reply === null) {
      $.ui.toast('No agent reply to like yet.')
      return { text: '' }
    }
    const isLiked = await setLike($, { text: reply.text, author: 'agent', authorName: MAIN_NAME }, YOU, 'toggle')
    $.ui.toast(isLiked ? '♥ Liked the last reply' : 'Unliked the last reply')
    return { text: '' }
  })

  on('command.run', { command: 'likes' }, async $ => {
    await $.ui.open({ id: PANE, title: 'Liked messages' })
    return { text: '' }
  })

  // A heart under every block of an agent reply, and who else liked it.
  on('ui.render', { component: 'AssistantMessage' }, async ($, e, next) => {
    if (e.props.isSummary || e.props.text.trim() === '') {
      return next(e)
    }
    const { Box, Text, Button, Markdown } = $.ui.resolve(e)
    const text = e.props.text
    const key = keyOf(text)
    const like = (await read($, likes))[key]
    const isLiked = like?.by[YOU] !== undefined
    const agents = agentLikers(like)
    const gutter = e.surface === 'terminal' ? (e.props.isFirstOfReply ? '⏺ ' : '  ') : ''

    return (
      <Box flexDirection="column">
        <Box flexDirection="row">
          {gutter !== '' && <Text>{gutter}</Text>}
          <Box flexDirection="column" flexGrow={1}>
            <Markdown text={text} />
          </Box>
        </Box>
        <Box flexDirection="row">
          {gutter !== '' && <Text>{'  '}</Text>}
          <Button
            key={`like:${key}`}
            plain
            dimColor={!isLiked}
            label={isLiked ? '♥ liked' : '♡'}
            onPress={() => void setLike($, { text, author: 'agent', authorName: MAIN_NAME }, YOU, 'toggle')}
          />
          {agents.length > 0 && <Text color="magenta"> ✦ liked by {agents.join(', ')}</Text>}
        </Box>
      </Box>
    )
  })

  // The person's own prompts: the engine draws them; an agent's like is a line beneath.
  on('ui.render', { component: 'UserMessage' }, async ($, e, next) => {
    if (e.props.isExpanded) {
      return next(e)
    }
    const agents = agentLikers((await read($, likes))[keyOf(e.props.text)])
    if (agents.length === 0) {
      return next(e)
    }
    return next({ ...e, props: { ...e.props, text: `${e.props.text}\n\n✦ liked by ${agents.join(', ')}` } })
  })

  on('ui.render', { component: 'Pane', requestId: PANE }, async ($, e) => {
    const { Box, Text, Button } = $.ui.resolve(e)
    const latest = (like: Like) => Math.max(...Object.values(like.by).map(m => m.at))
    const all = Object.values(await read($, likes)).sort((a, b) => latest(b) - latest(a))
    const room = Math.max(1, Math.floor(((e.viewport?.rows ?? 24) - 4) / 2))

    return (
      <Box flexDirection="column">
        {all.length === 0 && <Text dimColor>No liked messages yet. Press ♡ under a reply, or run /like.</Text>}
        {all.slice(0, room).map(like => {
          const isMine = like.by[YOU] !== undefined
          const notes = agentLikers(like).map(who => {
            const why = like.by[who]?.reason
            return why ? `${who}: ${why}` : who
          })
          return (
            <Box key={`row:${like.key}`} flexDirection="column">
              <Box flexDirection="row">
                {like.author === 'user' ? (
                  <Text color="magenta">✦</Text>
                ) : (
                  <Button
                    key={`unlike:${like.key}`}
                    plain
                    dimColor={!isMine}
                    label={isMine ? '♥' : '♡'}
                    onPress={() =>
                      void setLike($, { text: like.text, author: like.author, authorName: like.authorName }, YOU, 'toggle')
                    }
                  />
                )}
                <Text>
                  {' '}
                  <Text dimColor>{like.author === 'user' ? 'you' : (like.authorName ?? 'agent')}:</Text>{' '}
                  {firstLine(like.text)}
                </Text>
              </Box>
              {notes.length > 0 && <Text color="magenta">{'  '}✦ {notes.join(' · ')}</Text>}
            </Box>
          )
        })}
        {all.length > room && <Text dimColor>…and {all.length - room} more</Text>}
      </Box>
    )
  })
}
