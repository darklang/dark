<script setup lang="ts">
import type { text } from '@fortawesome/fontawesome-svg-core'
import { ref, onMounted } from 'vue'
import autosize from 'autosize'

const userInput = ref('Write a function that divides two numbers')
const content = ref()
const isLoading = ref(false)

async function submit() {
  try {
    isLoading.value = true
    console.log('emitting prompt to submit')
    const evt = { UserGavePrompt: [userInput.value] }
    const result = await window.darklang.handleEvent(evt)
    console.log('result', result)
    isLoading.value = false
    autosize.destroy(content.value)
    autosize(content.value)
  } catch (error) {
    console.error(error)
  }
}

onMounted(() => autosize(content.value))
</script>

<template>
  <div
    class="flex items-center justify-center m-4 fixed bottom-0 left-0 right-0 p-3 border border-white/10 rounded-md"
  >
    <textarea
      ref="content"
      v-model="userInput"
      autosize
      rows="1"
      type="text"
      placeholder="Type your message"
      class="w-full max-h-44 outline-none m-0 resize-none overflow-y-auto border-0 bg-transparent text-white py-0 pl-2 pr-11"
      @keydown.enter.exact.prevent="submit"
    ></textarea>
    <button
      @click="submit"
      :class="{ 'opacity-50 cursor-not-allowed': isLoading }"
      :disabled="isLoading"
      class="absolute bottom-2 right-2 py-1 px-2 mx-2 rounded-md text-white bg-[#C56AE4] hover:bg-[#9f56b8]"
    >
      <span v-if="!isLoading">send</span>
      <span v-else class="flex">
        <fa icon="fa-spinner" class="animate-spin w-6 p-1" />
      </span>
    </button>
  </div>
</template>
