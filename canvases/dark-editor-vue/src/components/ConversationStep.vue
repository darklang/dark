<script setup lang="ts">
import type { ChatHistoryItem, CodeSnippet } from '@/types'

import CodeSnippetComponent from './CodeSnippet.vue'

let props = defineProps<{
  chatItem: ChatHistoryItem
  codeSnippets: CodeSnippet[]
}>()

function snippetForId(id: string): CodeSnippet {
  // TODO: handle 'missing' snippet better
  return props.codeSnippets.find((snippet) => snippet.id === id)!
}
</script>

<template>
  <div
    class="flex flex-col p-2 mb-2 bg-gray-200 rounded"
    :class="{ 'bg-blue-300': chatItem.typ === 'Bot' }"
  >
    <p class="font-bold">
      {{ chatItem.typ === 'User' ? 'User' : 'Bot' }}
    </p>

    <div v-if="chatItem.typ === 'Bot'">
      <div v-for="(item, index) in chatItem.items">
        <CodeSnippetComponent
          v-if="item.typ === 'Code'"
          :key="item.id"
          :snippet="snippetForId(item.id)"
        />
        <p v-else>
          {{ item.text }}
        </p>
      </div>
    </div>

    <p v-else-if="chatItem.typ === 'User'">
      {{ chatItem.prompt }}
    </p>
  </div>
</template>
