/** Who liked: `you` for the person, else an agent's display name. */
export type Liker = string

export type LikeMark = {
  /** Epoch milliseconds. */
  at: number
  /** Why, when an agent said. */
  reason?: string
}

export type Like = {
  /** Content key of the message block (see `keyOf`). */
  key: string
  /** The block's markdown, cut to MAX_TEXT characters. */
  text: string
  /** Who wrote the liked block. */
  author: 'user' | 'agent'
  /** Display name of the writer, when an agent wrote it. */
  authorName?: string
  by: Record<Liker, LikeMark>
}

/** One message row seen this session, for agents to find by quote. */
export type Seen = {
  author: 'user' | 'agent'
  /** The loop that wrote it: `main`, a subagent's id, or `peer`. */
  loop: string
  authorName: string
  /** The row's text blocks. */
  texts: string[]
}

export type LastReply = { turnId: string; text: string }

declare module 'claude-code' {
  interface PluginState {
    'like-messages': {
      likes: Record<string, Like>
      seen: Seen[]
      lastReply: LastReply | null
    }
  }
}
