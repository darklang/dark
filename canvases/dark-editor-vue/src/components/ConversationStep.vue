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
    class="flex p-3 mb-2 ml-2 rounded text-white"
    :class="{ 'bg-[#222222] rounded-xl': chatItem.typ === 'Bot' }"
  >
    <div
      class="relative inline-flex items-center justify-center w-10 h-10 overflow-hidden rounded-full p-5"
      :class="{
        'bg-[#6756F6]': chatItem.typ === 'Bot',
        'bg-[#D356CD]': chatItem.typ === 'User',
      }"
    >
      <p class="font-bold">
        {{ chatItem.typ === 'User' ? 'User' : 'Bot' }}
      </p>
    </div>

    <div class="p-3 flex-grow" v-if="chatItem.typ === 'Bot'">
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

    <p class="p-3 flex-grow" v-else-if="chatItem.typ === 'User'">
      {{ chatItem.prompt }}
    </p>
  </div>
</template>
