<script setup lang="ts">
import type { ChatHistoryItem, CodeSnippet, Task } from '@/types'

import CodeSnippetComponent from './CodeSnippet.vue'

let props = defineProps<{
  chatItem: ChatHistoryItem
  codeSnippets: CodeSnippet[]
  tasks: Task[]
}>()

// function snippetForId(id: string): CodeSnippet {
//   // TODO: handle 'missing' snippet better
//   console.log('snippetForId', id)
//   return props.codeSnippets.find((snippet) => snippet.id === id)!
// }
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

    <div class="p-3 flex-grow" v-if="chatItem.typ === 'Bot'">
      <div v-for="(item, index) in chatItem.items">
        <!-- <CodeSnippetComponent
          v-if="item.typ === 'Code'"
          :key="item.id"
          :snippet="snippetForId(item.id)"
        /> -->
        <!-- <div v-if="item.typ === 'Code'">code in context and code view</div>
        <div v-else-if="item.typ === 'Tasks'">
          <h2 class="font-bold">Tasks</h2>
          tasks are displayed in tasks and actions view
          <ul>
            <li v-for="(task, index) in tasks" :key="index">
              {{ task.description }}
            </li>
          </ul>
        </div> -->
        <p v-if="item.typ === 'Text'">
          {{ item.text }}
        </p>
      </div>
    </div>

    <p class="p-3 flex-grow" v-else-if="chatItem.typ === 'User'">
      {{ chatItem.prompt }}
    </p>
  </div>
</template>
