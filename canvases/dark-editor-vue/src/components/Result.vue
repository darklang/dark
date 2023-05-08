<script setup lang="ts">
import { defineProps } from 'vue'

const props = defineProps({
  message: {
    type: String,
    required: true,
  },
})
</script>

<template>
  <div
    v-if="message"
    class="flex flex-col bg-[#2e2e2e] rounded mx-auto max-w-2xl text-white"
  >
    <div class="border-b p-3 border-[#404040]">
      <p class="text-[#C56AE4]">Result</p>
    </div>
    <div class="p-3">{{ message }}</div>
  </div>
</template>
