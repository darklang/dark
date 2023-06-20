// -- internal types --
export interface CodeSnippet {
  id: string
  code: string
  eval: string | null
}
export enum Status {
  Todo,
  InProgress,
  Done,
}

export interface Task {
  id: string
  description: string
  status: Status
}

export interface Action {
  id: string
  description: string
  status: Status
}

export type BotResponseItem =
  | { typ: 'Code'; id: string }
  | { typ: 'Text'; text: string }
  | { typ: 'Tasks'; tasks: Task[] }
  | { typ: 'Actions'; actions: Action[] }

export type ChatHistoryItem =
  | { typ: 'User'; id: string; prompt: string }
  | { typ: 'Bot'; id: string; items: BotResponseItem[] }

export interface Model {
  isLoading: boolean
  systemPrompt: string
  chatHistory: ChatHistoryItem[]
  codeSnippets: CodeSnippet[]
  tasks: Task[]
  actions: Action[]
  functions: string[]
}

// -- parse the internal model based on however Dark stuff is serialized --
//
// Trying to 'share' a type across the seam is really annoying due to
// how Dark serializes enums, vs how typescript hackily represents enums.
//
// Anyway, this seems good enough for now,
// and is the only place in either TypeScript or Dark that "isn't typed well"
//
// (Note: we might have to do something similar for outgoing Msgs soon, once one uses an enum)
export function fromSerializedDarkModel(serializedDarkModel: string): Model {
  let source = JSON.parse(serializedDarkModel)

  const chatHistory: ChatHistoryItem[] = source.chatHistory.map((h: any) => {
    let typ = Object.keys(h)[0]
    let data = h[typ]

    switch (typ) {
      case 'UserPrompt':
        return { typ: 'User', id: data[0], prompt: data[1] }

      case 'BotResponse':
        let mappedItems = data[1].map((item: any) => {
          let typ = Object.keys(item)[0]
          let data = item[typ]

          switch (typ) {
            case 'CodeSnippet':
              return { typ: 'Code', id: data[0] }

            case 'Text':
              return { typ: 'Text', text: data[0] }

            case 'Tasks':
              return {
                typ: 'Tasks',
                id: data[0],
              }

            case 'Actions':
              return {
                typ: 'Actions',
                id: data[0],
              }

            default:
              throw new Error(`unknown bot response item type ${typ}`)
          }
        })

        return { typ: 'Bot', id: data[0], items: mappedItems }

      default:
        throw new Error(`unknown chat history item type ${typ}`)
    }
  })

  const codeSnippets = source.codeSnippets.map((snip: any) => {
    let evalType = Object.keys(snip.eval)[0]
    let evalData = snip.eval[evalType]

    switch (evalType) {
      case 'Nothing':
        return { id: snip.id, code: snip.code, eval: null }

      case 'Just':
        return { id: snip.id, code: snip.code, eval: evalData }

      default:
        throw new Error(`unknown code snippet eval type ${evalType}`)
    }
  })

  const tasks = source.tasks.map((task: any) => {
    return { id: task.id, description: task.description, status: task.status }
  })

  const actions = source.actions.map((action: any) => {
    return { id: action.id, description: action.description, status: action.status }
  })

  return {
    isLoading: source.isLoading,
    systemPrompt: source.systemPrompt,
    chatHistory: chatHistory,
    codeSnippets: codeSnippets,
    tasks: tasks,
    actions: actions,
    functions: source.functions,
  }
}
