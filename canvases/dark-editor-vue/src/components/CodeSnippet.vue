<script setup lang="ts">
import { ref } from 'vue'

import type { CodeSnippet } from '@/types'

const props = defineProps<{
  snippet: CodeSnippet
}>()

const codeSnippet = ref(props.snippet.code)

async function runCode() {
  try {
    const evt = { UserRequestedCodeEval: [props.snippet.id, codeSnippet.value] }
    const result = await window.darklang.handleEvent(evt)
    console.log('result', result)
  } catch (error) {
    console.error(error)
  }
}
</script>

<template>
  <div class="p-4 bg-gray-100 rounded">
    <textarea
      v-model="codeSnippet"
      class="w-full h-24 p-2 text-xs bg-gray-200 rounded"
    ></textarea>
    <div class="flex justify-between mt-2">
      <button
        @click="runCode"
        class="px-4 py-2 text-xs font-bold text-white bg-blue-500 rounded"
      >
        eval
      </button>
    </div>

    <pre v-if="props.snippet.eval" class="text-xs bg-white rounded">
      {{ props.snippet.eval }}
    </pre>
  </div>
</template>
