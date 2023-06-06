<script setup lang="ts">
import type { Model } from '../types'

import Conversation from '../components/Conversation.vue'
import TasksAndActions from '../components/TasksAndActions.vue'
import CodeAndContext from '../components/CodeAndContext.vue'

let props = defineProps<{ state: Model }>()
</script>

<template>
  <main>
    <div class="flex h-screen overflow-hidden">
      <div class="w-1/5 overflow-auto pb-24">
        <TasksAndActions
          v-bind:state="state"
          :tasks="state.tasks"
          :actions="state.actions"
        />
      </div>
      <div class="w-2/5 overflow-auto pb-24">
        <Conversation :state="props.state" />
      </div>
      <div class="w-2/5 overflow-auto pb-32">
        <CodeAndContext v-bind:state="state" :codeSnippets="state.codeSnippets" />
      </div>
    </div>
  </main>
</template>
