<script setup lang="ts">
import { ref } from 'vue';

const props = defineProps({
  code: {
    type: String,
    default: '',
  },
});

const codeSnippet = ref(props.code);

async function runCode() {
  try {
    console.log('emitting code to run');
    const result = await window.darklang.handleEvent('runCode', codeSnippet.value);
    console.log('result', result)
  } catch (error) {
    console.error(error);
  }
}
</script>

<template>
  <div class="p-4 bg-gray-100 rounded">
    <textarea
      v-model="codeSnippet"
      class="w-full h-24 p-2 text-xs bg-gray-200 rounded"
    ></textarea>
    <div class="flex justify-between mt-2">
      <button
        @click="runCode"
        class="px-4 py-2 text-xs font-bold text-white bg-blue-500 rounded"
      >
        Play
      </button>
      <pre class="flex-1 p-2 text-xs bg-white rounded">
        <!-- Render code execution results here -->
      </pre>
    </div>
  </div>
</template>