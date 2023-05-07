<template>
  <main>
    <Header />
    <Accordion class="mx-auto">
      <template v-slot:accordion-header>
        <span class="font-semibold text-xl">System prompt</span>
        <svg class="w-6 h-6 ml-2 transform transition-transform" viewBox="0 0 24 24">
          <path
            fill="currentColor"
            d="M12 16.5l6-6-1.5-1.5-4.5 4.5-4.5-4.5-1.5 1.5z"
          />
        </svg>
      </template>
      <template v-slot:accordion-body>
        <textarea
          class="bg-transparent outline-0 p-4 w-full resize-none h-96 max-h-96 overflow-y-auto border-0 text-white"
          ref="prompt"
          v-model="systemPromptValue"
        ></textarea>
      </template>
    </Accordion>

    <div>
      <Prompt :systemPromptValue="systemPromptValue" />
    </div>
  </main>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import { onMounted } from '@vue/runtime-core'

import Header from '../components/Header.vue'
import Accordion from '../components/Accordion.vue'
import Prompt from '../components/Prompt.vue'

const systemPromptValue = ref('')

const darklangJSScript: HTMLScriptElement = document.createElement('script')
darklangJSScript.setAttribute(
  'src',
  'http://dark-serve-static.dlio.localhost:11003/darklang-wasm.js'
)
darklangJSScript.setAttribute('defer', '')
darklangJSScript.addEventListener('load', async () => {
  const darklang = await window.Darklang.init()
  window.darklang = darklang
})
document.head.appendChild(darklangJSScript)

onMounted(() => {
  fetch('/get-prompt')
    .then((response) => response.text())
    .then((data) => (systemPromptValue.value = data))
})
</script>
