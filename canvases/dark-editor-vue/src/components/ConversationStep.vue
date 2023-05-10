<script setup lang="ts">
import CodeSnippet from './CodeSnippet.vue';

// TODO: property type this
const props = defineProps({
  chatItem: {
    type: Object,
    default: '',
  },
});
</script>

<template>
  <div
    class="flex flex-col p-2 mb-2 bg-gray-200 rounded"
    :class="{ 'bg-blue-300': chatItem.author === 'Bot' }"
  >
    <p class="font-bold">
      {{ chatItem.author === 'User' ? 'User' : 'Bot' }}
    </p>
    <div v-if="chatItem.author === 'Bot' && chatItem.isCode">
      <CodeSnippet :code="chatItem.text" />
    </div>
    <p v-else>{{ chatItem.text }}</p>
  </div>
</template>