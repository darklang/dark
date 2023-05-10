<script setup lang="ts">
import CodeSnippet from './CodeSnippet.vue';

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
    :class="{ 'bg-blue-300': chatItem.Author === 'Bot' }"
  >
    <p class="font-bold">
      {{ chatItem.Author === 'User' ? 'User' : 'Bot' }}
    </p>
    <div v-if="chatItem.Author === 'Bot' && chatItem.IsCode">
      <CodeSnippet :code="chatItem.Text" />
    </div>
    <p v-else>{{ chatItem.Text }}</p>
  </div>
</template>