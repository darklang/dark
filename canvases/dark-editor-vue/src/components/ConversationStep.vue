<script setup lang="ts">
import type { ChatHistoryItem, CodeSnippet, Task } from '@/types'

let props = defineProps<{
  chatItem: ChatHistoryItem
  codeSnippets: CodeSnippet[]
  tasks: Task[]
}>()
</script>

<template>
  <div
    class="flex p-3 mb-2 ml-2 rounded text-white"
    :class="{ 'bg-[#222222] rounded-xl overflow-scroll': chatItem.typ === 'Bot' }"
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

    <div class="pl-3 flex-grow" v-if="chatItem.typ === 'Bot'">
      <div v-for="(item, index) in chatItem.items">
        <pre class="whitespace-pre-wrap break-w text-sm" v-if="item.typ === 'Text'"
          >{{ item.text }} </pre
        >
      </div>
    </div>

    <div
      class="pl-3 flex items-center flex-grow"
      v-else-if="chatItem.typ === 'User'"
    >
      <pre class="whitespace-pre-wrap break-w text-sm">{{ chatItem.prompt }}</pre>
    </div>
  </div>
</template>
