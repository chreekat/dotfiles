export type Like = {
  /** The transcript message id, or `turn:<turnId>` when liked via /like. */
  id: string
  /** The message's markdown, cut to MAX_TEXT characters. */
  text: string
  /** Epoch milliseconds. */
  likedAt: number
}

export type LastReply = { turnId: string; text: string }

declare module 'claude-code' {
  interface PluginState {
    'like-messages': {
      likes: Record<string, Like>
      lastReply: LastReply | null
    }
  }
}
