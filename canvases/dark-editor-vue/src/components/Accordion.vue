<template>
  <div
    class="text-white flex flex-col stretch my-4 mx-auto max-w-2xl shadow-lg shadow-black/10"
  >
    <div
      @click="toggleAccordion()"
      class="isOpen p-4 cursor-pointer flex items-center justify-between border-2 border-white/10 hover:border-[#C56AE4] rounded-md mb-2"
    >
      <slot name="accordion-header"></slot>
    </div>
    <div v-show="isOpen" class="border-2 rounded-md border-white/10">
      <slot name="accordion-body"></slot>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref } from 'vue'

const isOpen = ref(false)

function toggleAccordion() {
  isOpen.value = !isOpen.value
}
</script>
