<script setup lang="ts">
import { ref } from 'vue'

const userInput = ref('Write a function that divides two numbers')

async function submit() {
  try {
    console.log('emitting prompt to submit')
    const evt = { UserGavePrompt: [userInput.value] }
    const result = await window.darklang.handleEvent(evt)
    console.log('result', result)
  } catch (error) {
    console.error(error)
  }
}
</script>

<template>
  <div class="fixed bottom-0 left-0 right-0 p-4 bg-gray-200">
    <input
      v-model="userInput"
      type="text"
      placeholder="Type your message"
      class="w-full px-3 py-2 text-sm bg-white rounded"
    />
    <button
      @click="submit"
      class="absolute top-2 right-4 px-4 py-2 text-xs font-bold text-white bg-blue-500 rounded"
    >
      Submit
    </button>
  </div>
</template>
