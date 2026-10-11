import { expect, mock, test } from 'claude-code/testing'

const REPLY = {
  component: 'AssistantMessage',
  requestId: 'msg-1',
  props: { text: 'Here is **the answer**.', isFirstOfReply: true },
} as const

test('the heart under a reply toggles on every surface', async ($, on) => {
  mock.clock(on)
  mock.store(on)
  for (const surface of ['terminal', 'desktop'] as const) {
    const ui = await $.ui.mount({ plugin: 'like-messages', surface, ...REPLY })
    const key = 'like:msg-1'
    expect((await ui.find({ key }))?.props.label).toBe('♡')
    await ui.press({ key })
    expect((await ui.find({ key }))?.props.label).toBe('♥ liked')
    await ui.press({ key })
    expect((await ui.find({ key }))?.props.label).toBe('♡')
    await ui.unmount()
  }
})

test('a liked reply shows in the likes pane', async ($, on) => {
  mock.clock(on)
  mock.store(on)
  const reply = await $.ui.mount({ plugin: 'like-messages', surface: 'terminal', ...REPLY })
  await reply.press({ key: 'like:msg-1' })
  const pane = await $.ui.mount({
    plugin: 'like-messages',
    surface: 'terminal',
    component: 'Pane',
    requestId: 'likes',
    props: {} as never,
  })
  expect(await pane.find({ type: 'Text', text: /the answer/ })).toBeDefined()
  await pane.press({ key: 'unlike:msg-1' })
  expect(await pane.find({ type: 'Text', text: /No liked messages/ })).toBeDefined()
})
