<script setup lang="ts">
import { ref, onMounted } from 'vue'

import MainView from './views/MainView.vue'

const state = ref({
  SystemPrompt: 'TODO',
})

/** Listen for state updates from Dark */
window.stateUpdated = (newState: any) => {
  state.value = JSON.parse(newState)
}

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
  <MainView :state="state" />
</template>
