<script setup lang="ts">
import type { ChatHistoryItem, CodeSnippet } from '@/types'
import { defineProps } from 'vue'
import CodeSnippetComponent from '@/components/CodeSnippet.vue'
import type { Model } from '../types'

let props = defineProps<{
  state: Model
  codeSnippets: CodeSnippet[]
}>()

function snippetForId(id: string): CodeSnippet {
  return props.codeSnippets.find((snippet) => snippet.id === id)!
}
</script>

<template>
  <div class="min-h-screen bg-[#151515] text-white">
    <h1 class="p-2 mt-2 font-semibold">Context And Code</h1>
    <div class="p-2 m-2">
      <h2 class="font-semibold">Context</h2>
    </div>
    <h2 class="p-2 m-2 font-semibold">Code</h2>
    <div v-for="(item, index) in codeSnippets" :key="index">
      <CodeSnippetComponent :key="item.id" :snippet="snippetForId(item.id)" />
    </div>
  </div>
</template>
