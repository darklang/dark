<template>
  <main>
    <Header />
    <Accordion class="mx-auto">
      <template v-slot:accordion-header>
        <span class="font-semibold text-xl">System prompt</span>
      </template>
      <template v-slot:accordion-body>
        <textarea class="bg-transparent outline-0 resize-none w-full">
          TODO: Fetch dark prompt here
        </textarea>
      </template>
    </Accordion>
    <div id="chat_container">
    </div>
    <div class="absolute bottom-0 left-0 w-full pt-2">
    <Prompt />
    </div>
  </main>
</template>

<script setup lang="ts">
  import Header from "../components/Header.vue";
  import Accordion from "../components/Accordion.vue";
  import Prompt from "../components/Prompt.vue";

</script>

