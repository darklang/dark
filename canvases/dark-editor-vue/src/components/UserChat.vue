<template>
  <div class="my-4 mx-auto max-w-2xl">
    <div class="flex items-start">
      <div class="relative inline-flex items-center justify-center w-10 h-10 overflow-hidden bg-gray-100 rounded-full dark:bg-gray-600">
    <span class="font-medium text-gray-600 dark:text-gray-300">User</span>
  </div>
      <div class="ml-3 w-fit">
        <p class="text-white">{{ promptValue }}</p>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
export default {
  props: {
    promptValue: {
    type: String,
    required: true,
  },
  },
};
</script>


