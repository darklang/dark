<template>
  <main>
    <Header />
    <Accordion class="mx-auto">
      <template v-slot:accordion-header>
        <span class="font-semibold text-xl">System prompt</span>
      </template>
      <template v-slot:accordion-body>
        <textarea class="bg-transparent outline-0 resize-none w-full" ref="prompt" v-model="systemPromptValue"></textarea>
      </template>
    </Accordion>
    <div id="chat_container">
      <!-- <Test/> -->

    </div>
    <div class="absolute bottom-0 left-0 w-full pt-2">
      <Prompt :systemPromptValue="systemPromptValue"  />
    </div>
  </main>
</template>

<script setup lang="ts">
  import Header from "../components/Header.vue";
  import Accordion from "../components/Accordion.vue";
  import Prompt from "../components/Prompt.vue";
  // import Test from "../components/Test.vue";
  import { ref } from 'vue';
  import { onMounted } from '@vue/runtime-core';
  const systemPromptValue= ref("");
  onMounted(() => {
  fetch('/get-prompt')
  .then(response => response.text())
  .then(data => systemPromptValue.value = data);
});
</script>