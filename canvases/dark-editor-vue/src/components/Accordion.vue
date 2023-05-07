<template>
  <div
    class="text-white rounded-md flex flex-col stretch my-4 mx-auto max-w-2xl border border-[#C56AE4] shadow-lg shadow-black/10"
  >
    <div
      @click="toggleAccordion()"
      class="isOpen p-4 cursor-pointer flex items-center"
    >
      <slot name="accordion-header"></slot>
    </div>
    <div v-show="isOpen">
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
