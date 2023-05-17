<script setup lang="ts">
import { ref } from 'vue'

import type { Model } from '../types'

import ConversationStep from './ConversationStep.vue'
import Prompt from './Prompt.vue'

defineProps<{ state: Model }>()

const isPromptVisible = ref(false)
</script>

<template>
  <div class="min-h-screen bg-[#151515]">
    <div class="p-4 pb-20">
      <!-- system prompt -->
      <div class="rounded text-white">
        <input
          type="checkbox"
          id="system-prompt"
          class="absolute left-0 hidden"
          v-model="isPromptVisible"
        />
        <label
          for="system-prompt"
          class="p-4 cursor-pointer flex items-center justify-between border-2 border-white/10 hover:border-[#C56AE4] rounded-md mb-2"
        >
          System Prompt
          <fa icon="fa-angle-down" class="w-4 h-4 ml-2" />
        </label>
        <div v-show="isPromptVisible">
          <textarea
            v-model="state.systemPrompt"
            class="border-2 rounded-md border-white/10 bg-transparent outline-0 p-4 w-full resize-none h-96 max-h-96 overflow-y-auto text-white"
            rows="4"
          ></textarea>
        </div>
      </div>

      <!-- actual conversation -->
      <div class="mt-4 space-y-4">
        <ConversationStep
          v-for="chatItem in state.chatHistory"
          :key="chatItem.id"
          :chatItem="chatItem"
          :codeSnippets="state.codeSnippets"
        />
      </div>
    </div>

    <!-- User Prompt Input at the bottom -->
    <Prompt />
  </div>
</template>
