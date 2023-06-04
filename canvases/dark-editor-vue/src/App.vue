<script setup lang="ts">
import { ref } from 'vue'

import { fromSerializedDarkModel, type Model } from './types'
import Header from './components/Header.vue'
import ConversationView from './views/ConversationView.vue'
import TasksAndActionsView from './views/TasksAndActionsView.vue'
import CodeAndContextView from './views/CodeAndContextView.vue'

let init: Model = {
  systemPrompt: '<system prompt here>!',
  chatHistory: [],
  codeSnippets: [],
  tasks: [],
  actions: [],
}

// Set initial state; listen for state updates from Dark
const state = ref(init)
window.stateUpdated = (serializedNewState: string) => {
  //console.log('stateUpdated', serializedNewState)
  try {
    const parsed = fromSerializedDarkModel(serializedNewState)
    state.value = parsed
  } catch (e) {
    console.error('Failed to parse updated state', e)
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
  const darklang = await window.Darklang.init('dark-editor', window.stateUpdated)

  // TODO: we don't need to expose this onace the logic in ResponseChat.vue is
  // ported to Dark.
  window.darklang = darklang
})

document.head.appendChild(darklangJSScript)
</script>

<template>
  <div class="h-screen overflow-hidden">
    <Header />
    <div class="flex h-screen overflow-hidden">
      <div class="w-1/5 overflow-auto pb-24">
        <TasksAndActionsView
          v-bind:state="state"
          :tasks="state.tasks"
          :actions="state.actions"
        />
      </div>
      <div class="w-2/5 overflow-auto pb-24">
        <ConversationView v-bind:state="state" />
      </div>
      <div class="w-2/5 overflow-auto pb-24">
        <CodeAndContextView
          v-bind:state="state"
          :codeSnippets="state.codeSnippets"
        />
      </div>
    </div>
  </div>
</template>
