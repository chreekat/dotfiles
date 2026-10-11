import { expect, mock, test } from 'claude-code/testing'
import type { On } from 'claude-code'
import type { Engine } from 'claude-code/testing'

const REPLY = {
  component: 'AssistantMessage',
  requestId: 'msg-1',
  props: { text: 'Here is **the answer**.', isFirstOfReply: true },
} as const

const PANE = { component: 'Pane', requestId: 'likes', props: {} as never } as const

const TOOL = 'mcp__like-messages__like'

function world(on: On) {
  mock.clock(on)
  mock.store(on)
  on('ui.toast', () => ({ value: undefined }))
  on('agent.list', () => ({ value: [{ id: 'sub-123456', type: 'reviewer', description: 'review the diff', status: 'running' }] }))
  // The engine's own drawing of a prompt row, as the test stands for it.
  on('ui.render', { component: 'UserMessage' }, ($, e) => {
    const { Text } = $.ui.resolve(e)
    return <Text>{e.props.text}</Text>
  })
}

async function userSays($: Engine, uuid: string, text: string) {
  await $.session.append({
    message: { type: 'user', role: 'user', content: [{ type: 'text', text }] },
    door: 'prompt',
    origin: { kind: 'composer' },
    uuid,
  })
}

async function agentSays($: Engine, uuid: string, text: string, agentId?: string) {
  await $.session.append({
    message: { type: 'assistant', role: 'assistant', content: [{ type: 'text', text }] },
    door: 'response',
    origin: { kind: 'model', model: 'test' },
    uuid,
    ...(agentId ? { agentId } : {}),
  })
}

test('the heart under a reply toggles on every surface', async ($, on) => {
  world(on)
  for (const surface of ['terminal', 'desktop', 'vscode', 'mobile'] as const) {
    const ui = await $.ui.mount({ plugin: 'like-messages', surface, ...REPLY })
    const heart = async () => (await ui.find({ type: 'Button' }))?.props.label
    const key = (await ui.find({ type: 'Button' }))?.key ?? ''
    expect(await heart()).toBe('♡')
    await ui.press({ key })
    expect(await heart()).toBe('♥ liked')
    await ui.press({ key })
    expect(await heart()).toBe('♡')
    await ui.unmount()
  }
})

test('agent likes and the pane draw on the phone', async ($, on) => {
  world(on)
  await agentSays($, 'a1', 'Here is **the answer**.', 'sub-123456')
  await $.tool.call({ tool: TOOL, quote: 'the answer', reason: 'nice' })
  const reply = await $.ui.mount({ plugin: 'like-messages', surface: 'mobile', ...REPLY })
  expect(await reply.find({ type: 'Text', text: /liked by Claude/ })).toBeDefined()
  const pane = await $.ui.mount({ plugin: 'like-messages', surface: 'mobile', ...PANE })
  expect(await pane.find({ type: 'Text', text: /Claude: nice/ })).toBeDefined()
})

test('a liked reply shows in the likes pane, and unlikes from there', async ($, on) => {
  world(on)
  const reply = await $.ui.mount({ plugin: 'like-messages', surface: 'terminal', ...REPLY })
  await reply.press({ key: (await reply.find({ type: 'Button' }))?.key ?? '' })
  const pane = await $.ui.mount({ plugin: 'like-messages', surface: 'terminal', ...PANE })
  expect(await pane.find({ type: 'Text', text: /the answer/ })).toBeDefined()
  await pane.press({ key: (await pane.find({ type: 'Button' }))?.key ?? '' })
  expect(await pane.find({ type: 'Text', text: /No liked messages/ })).toBeDefined()
})

test("the agent likes the user's message by quote, with a reason", async ($, on) => {
  world(on)
  await userSays($, 'u1', 'Could we try a  zipper\nover the tree instead?')
  const ran = await $.tool.call({ tool: TOOL, quote: 'a zipper over the tree', reason: 'elegant idea' })
  expect(ran.text ?? String(ran.result)).toMatch(/Liked the user's message/)

  const row = await $.ui.mount({
    plugin: 'like-messages',
    surface: 'terminal',
    component: 'UserMessage',
    props: { text: 'Could we try a  zipper\nover the tree instead?', origin: { kind: 'composer' }, isExpanded: false },
  })
  expect(await row.find({ type: 'Text', text: /zipper[\s\S]*liked by Claude/ })).toBeDefined()
  const pane = await $.ui.mount({ plugin: 'like-messages', surface: 'terminal', ...PANE })
  expect(await pane.find({ type: 'Text', text: /Claude: elegant idea/ })).toBeDefined()
})

test("an agent likes another agent's reply, and the reply shows it", async ($, on) => {
  world(on)
  await agentSays($, 'a1', 'Here is **the answer**.', 'sub-123456')
  const ran = await $.tool.call({ tool: TOOL, quote: 'the answer' })
  expect(ran.text ?? String(ran.result)).toMatch(/Liked/)

  const reply = await $.ui.mount({ plugin: 'like-messages', surface: 'terminal', ...REPLY })
  expect(await reply.find({ type: 'Text', text: /liked by Claude/ })).toBeDefined()
  const pane = await $.ui.mount({ plugin: 'like-messages', surface: 'terminal', ...PANE })
  expect(await pane.find({ type: 'Text', text: /reviewer \(review the diff\)/ })).toBeDefined()
})

test('an agent cannot like its own message, nor one that is not there', async ($, on) => {
  world(on)
  await agentSays($, 'a1', 'I wrote this myself.')
  const own = await $.tool.call({ tool: TOOL, quote: 'wrote this myself' })
  expect(own.text ?? String(own.result)).toMatch(/your own/)
  const missing = await $.tool.call({ tool: TOOL, quote: 'never said' })
  expect(missing.text ?? String(missing.result)).toMatch(/No message/)
})
