<script setup lang="ts">
import { ref } from 'vue'

import { fromSerializedDarkModel, type Model } from './types'
import ConversationView from './views/ConversationView.vue'

let init: Model = {
  systemPrompt: '<system prompt here>!',
  chatHistory: [],
  codeSnippets: [],
}

// Set initial state; listen for state updates from Dark
const state = ref(init)
window.stateUpdated = (serializedNewState: string) => {
  //console.log('stateUpdated', serializedNewState)
  try {
    const parsed = fromSerializedDarkModel(serializedNewState)
    state.value = parsed
  } catch (e) {
    console.error("Failed to parse updated state", e)
  }
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
