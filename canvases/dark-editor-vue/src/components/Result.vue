<template>
  <div>
    <div v-if="message" class="text-white">{{ message }}</div>
  </div>
</template>
<script setup lang="ts">
import { defineProps } from 'vue'

const props = defineProps({
  message: {
    type: String,
    required: true,
  },
})
</script>
