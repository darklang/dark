<!-- <script setup lang="ts">
import { defineProps, ref, onMounted, watch, defineEmits, computed } from 'vue'
import autosize from 'autosize'

const props = defineProps({
  response: {
    type: String,
    required: true,
  },
})

const content = ref(null)

const localResponse = computed({
  get: () => props.response,
  // update the parent component whenever the textarea value changes
  set: (newValue) => {
    if (newValue !== props.response) {
      emits('updateResponse', newValue)
    }
  },
})

const emits = defineEmits(['updateResponse'])

onMounted(() => {
  if (content.value) {
    autosize(content.value)
  }
})

// Watch for changes to the response prop and update the textarea size accordingly
watch(
  () => props.response,
  (newValue) => {
    if (content.value) {
      autosize.update(content.value)
    }
  }
)
</script>

<template>
  <textarea
    ref="content"
    class="text-white bg-transparent w-full outline-none"
    v-model="localResponse"
  ></textarea>
</template> -->
