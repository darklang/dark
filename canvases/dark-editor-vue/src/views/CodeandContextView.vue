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
  console.log('snippetForIdcode', id)
  return props.codeSnippets.find((snippet) => snippet.id === id)!
}
</script>

<template>
  <div class="min-h-screen bg-[#151515] text-white">
    <h1>Context & Code</h1>
    <h2>Context</h2>
    <h2>Code</h2>
    <div v-for="(item, index) in codeSnippets" :key="index">
      <CodeSnippetComponent :key="item.id" :snippet="snippetForId(item.id)" />
    </div>
  </div>
</template>
