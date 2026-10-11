import { atom, read, update } from 'claude-code'
import type { EngineInterface, Register } from 'claude-code'

import type { Like } from '../types'

const PANE = 'likes'
const STORE_KEY = 'likes'
const MAX_TEXT = 4000

const likes = atom({ plugin: 'like-messages', key: 'likes' } as const, {})
const lastReply = atom({ plugin: 'like-messages', key: 'lastReply' } as const, null)

const firstLine = (text: string) => {
  const line = text.split('\n').find(l => l.trim() !== '') ?? ''
  return line.length > 100 ? `${line.slice(0, 99)}…` : line
}

// Write-through: $.state drives the drawing, $.store keeps it past the session.
async function toggle($: EngineInterface, id: string, text: string) {
  const now = await $.clock.now()
  let isLiked = false
  await update($, likes, all => {
    const next = { ...all }
    if (next[id]) {
      delete next[id]
    } else {
      next[id] = { id, text: text.slice(0, MAX_TEXT), likedAt: now }
      isLiked = true
    }
    return next
  })
  await $.store.set(STORE_KEY, await read($, likes))
  return isLiked
}

export const register: Register = on => {
  on('session.start', async ($, e, next) => {
    const stored = (await $.store.get(STORE_KEY)) as Record<string, Like> | undefined
    if (stored) {
      await update($, likes, all => ({ ...stored, ...all }))
    }
    await $.command.register({
      name: 'like',
      description: 'Like (or unlike) the last agent reply',
    })
    await $.command.register({
      name: 'likes',
      description: 'Show the agent messages you liked',
    })
    return next(e)
  })

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
    const isLiked = await toggle($, `turn:${reply.turnId}`, reply.text)
    $.ui.toast(isLiked ? '♥ Liked the last reply' : 'Unliked the last reply')
    return { text: '' }
  })

  on('command.run', { command: 'likes' }, async $ => {
    await $.ui.open({ id: PANE, title: 'Liked messages' })
    return { text: '' }
  })

  // A heart under every block of an agent reply.
  on('ui.render', { component: 'AssistantMessage' }, async ($, e, next) => {
    if (e.props.isSummary || e.props.text.trim() === '') {
      return next(e)
    }
    const { Box, Text, Button, Markdown } = $.ui.resolve(e)
    const id = e.requestId
    const isLiked = (await read($, likes))[id] !== undefined
    const text = e.props.text
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
            key={`like:${id}`}
            plain
            dimColor={!isLiked}
            label={isLiked ? '♥ liked' : '♡'}
            onPress={() => void toggle($, id, text)}
          />
        </Box>
      </Box>
    )
  })

  on('ui.render', { component: 'Pane', requestId: PANE }, async ($, e) => {
    const { Box, Text, Button } = $.ui.resolve(e)
    const all = Object.values(await read($, likes)).sort((a, b) => b.likedAt - a.likedAt)
    const room = Math.max(1, (e.viewport?.rows ?? 24) - 4)

    return (
      <Box flexDirection="column">
        {all.length === 0 && <Text dimColor>No liked messages yet. Press ♡ under a reply, or run /like.</Text>}
        {all.slice(0, room).map(like => (
          <Box key={`row:${like.id}`} flexDirection="row">
            <Button
              key={`unlike:${like.id}`}
              plain
              label="♥"
              onPress={() => void toggle($, like.id, like.text)}
            />
            <Text> {firstLine(like.text)}</Text>
          </Box>
        ))}
        {all.length > room && <Text dimColor>…and {all.length - room} more</Text>}
      </Box>
    )
  })
}
