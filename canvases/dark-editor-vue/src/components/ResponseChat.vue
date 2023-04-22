<template>
  <div class="my-4 mx-auto max-w-2xl">
    <div class="flex items-start">
      <div class="relative inline-flex items-center justify-center w-10 h-10 overflow-hidden bg-gray-100 rounded-full dark:bg-gray-600">
        <span class="font-medium text-gray-600 dark:text-gray-300">AI</span>
      </div>
      <div class="ml-3 w-fit">
        <p class="text-white">{{ response }}</p>
      </div>
    </div>
    <form @submit.prevent="submitForm" class="flex mt-2 justify-center">
      <div v-if="variables" v-for="(variable, index) in variables" :key="index">
        <label :for="'input-' + index" class="text-white ml-2">{{ variable }}</label>
        <input :id="'input-' + index" type="text" v-model="variableValues[index]" class="text-white bg-transparent my-2 ml-4 border-b border-white outline-none" />
      </div>
      <button type="submit" class="bg-[#C56AE4] text-white m-2 px-2 py-1 rounded">Submit</button>
    </form>
  </div>
</template>

<script setup lang="ts">
import { ref, defineProps } from 'vue';

const props = defineProps({
  response: {
    type: String,
    required: true,
  },
});

const variables = ref<string[]>([]);
const variableValues = ref<string[]>([]);

let resp = props.response
const matchResult = resp.match(/variables:\n([\s\S]*)/);
const responseVariables = matchResult ? matchResult[1].trim().split('\n').filter(Boolean) : [];
console.log(responseVariables);
variables.value.push(...responseVariables);

const submitForm = () => {
  console.log(variableValues.value);
};
</script>