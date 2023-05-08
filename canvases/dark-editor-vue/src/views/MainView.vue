<script setup lang="ts">
import { ref } from 'vue'
import { onMounted } from '@vue/runtime-core'

import PageHeader from '../components/common/PageHeader.vue'
import Accordion from '../components/common/Accordion.vue'
import Conversation from '../components/Conversation.vue'

const systemPromptValue = ref('')

const darklangJSScript: HTMLScriptElement = document.createElement('script')
darklangJSScript.setAttribute(
  'src',
  'http://dark-serve-static.dlio.localhost:11003/editor-bootstrap.js'
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

<template>
  <main>
    <!-- Dark logo etc at the top -->
    <PageHeader />

    <!-- System prompt -->
    <Accordion class="mx-auto">
      <template v-slot:accordion-header>
        <span class="font-semibold text-xl">System prompt</span>
        <fa icon="fa-angle-down" class="w-4 h-4 ml-2" />
      </template>
      <template v-slot:accordion-body>
        <textarea
          class="bg-transparent outline-0 p-4 w-full resize-none h-96 max-h-96 overflow-y-auto border-0 text-white"
          ref="prompt"
          v-model="systemPromptValue"
        ></textarea>
      </template>
    </Accordion>

    <!-- Conversation -->
    <div>
      <Conversation :systemPromptValue="systemPromptValue" />
    </div>
  </main>
</template>