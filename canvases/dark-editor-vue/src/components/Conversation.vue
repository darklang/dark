<script setup lang="ts">
import { ref } from 'vue'
import ConversationStep from './ConversationStep.vue'
import Prompt from './Prompt.vue'

defineProps({
  state: {
    type: Object, // todo
    required: true,
  },
})

const isPromptVisible = ref(false)
</script>

<template>
  <div class="min-h-screen bg-gray-100">
    <div class="p-4">
      <!-- system prompt -->
      <div class="bg-white rounded">
        <input
          type="checkbox"
          id="system-prompt"
          class="absolute left-0 hidden"
          v-model="isPromptVisible"
        />
        <label
          for="system-prompt"
          class="block p-2 mb-2 font-bold text-white bg-blue-500 rounded-t cursor-pointer"
        >
          System Prompt
        </label>
        <div v-show="isPromptVisible" class="p-4">
          <textarea
            v-model="state.SystemPrompt"
            class="w-full p-2 border border-gray-300 rounded"
            rows="4"
          ></textarea>
        </div>
      </div>

      <!-- actual conversation -->
      <div class="mt-4 space-y-4">
        <ConversationStep
          v-for="(chatItem, index) in state.ChatHistory"
          :key="index"
          :chatItem="chatItem"
        />
      </div>
    </div>

    <!-- User Prompt Input at the bottom -->
    <Prompt />
  </div>
</template>
