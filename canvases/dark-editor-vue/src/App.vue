<script setup lang="ts">
import { ref } from 'vue'
import ConversationView from './views/ConversationView.vue'


/*
interface BotResponseItem_Code  {
  typ: 'Code'
  codeSnippet: string
}
interface BotResponseItem_Text {
  typ: 'Text'
  text: string
}
type BotResponseItem = BotResponseItem_Code | BotResponseItem_Text

interface ChatHistoryItem_Bot {
  typ: 'Bot'

  // todo: extract out other type
  text: string
  isCode: boolean
}
interface ChatHistoryItem_User {
  typ: 'User'
  prompt: string
}
type ChatHistoryItem = ChatHistoryItem_Bot | ChatHistoryItem_User
*/

interface ChatHistoryItem {
  author: 'User' | 'Bot'
  isCode: boolean
  text: string
}

interface Model {
  systemPrompt: string
  chatHistory: ChatHistoryItem[]
}

let init: Model = {
  systemPrompt: '<system prompt here>!',
  chatHistory: [],
}

// Set initial state; listen for state updates from Dark
const state = ref(init)
window.stateUpdated = (newState: any) => {
  state.value = JSON.parse(newState)
  console.log('newState', newState)
}

// Bootstrap and connect the Dark side of the app
// (running in WebAssembly)
const darklangJSScript: HTMLScriptElement = document.createElement('script')
darklangJSScript.setAttribute(
  'src',
  'http://dark-serve-static.dlio.localhost:11003/editor-bootstrap.js'
)
darklangJSScript.setAttribute('defer', '')
darklangJSScript.addEventListener('load', async () => {
  // TODO: maybe do this on `onMounted`?
  const darklang = await window.Darklang.init()

  // TODO: we don't need to expose this onace the logic in ResponseChat.vue is
  // ported to Dark.
  // (while we're at it, we could collapse .init() and .loadClient() into one thing)
  window.darklang = darklang

  await window.darklang.loadClient(
    'http://dark-editor.dlio.localhost:11003/client.dark'
  )
})

document.head.appendChild(darklangJSScript)
</script>

<template>
  <ConversationView v-bind:state="state" />
</template>
