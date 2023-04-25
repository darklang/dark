<template>
  <textarea ref="content" class="text-white bg-transparent w-full outline-none">{{ response }}</textarea>
</template>

<script setup lang="ts">
import { defineProps, ref, onMounted, watch } from "vue";
import autosize from "autosize";

const props = defineProps({
  response: {
    type: String,
    required: true,
  },
});

const content = ref(null);

onMounted(() => {
  if (content.value) {
    autosize(content.value);
  }
});

// Watch for changes to the response prop and update the textarea size accordingly
watch(() => props.response, () => {
  if (content.value) {
    autosize.update(content.value);
  }
});

</script>
