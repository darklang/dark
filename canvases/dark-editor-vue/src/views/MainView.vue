<template>
  <main>
    <Header />
    <Accordion class="mx-auto">
      <template v-slot:accordion-header>
        <span class="font-semibold text-xl">System prompt</span>
      </template>
      <template v-slot:accordion-body>
        <textarea class="bg-transparent outline-0 p-4 w-full" ref="prompt" v-model="systemPromptValue"></textarea>
      </template>
    </Accordion>

    <div>
      <Prompt :systemPromptValue="systemPromptValue" />
    </div>
  </main>
</template>

<script setup lang="ts">
  import Header from "../components/Header.vue";
  import Accordion from "../components/Accordion.vue";
  import Prompt from "../components/Prompt.vue";
  import { ref } from 'vue';
  import { onMounted } from '@vue/runtime-core';
  const systemPromptValue= ref("");
  const blazorScript: HTMLScriptElement = document.createElement("script");
  blazorScript.setAttribute(
    "src",
    "http://dark-serve-blazor-assets.dlio.localhost:11003/blazor.webassembly.js",
  );
  blazorScript.setAttribute("autostart", "false");
  blazorScript.setAttribute("defer", "");
  blazorScript.addEventListener('load', async () => {
    await Blazor.start({
      loadBootResource: function (type: string, name: string, defaultUri: string, integrity?: string) {
        return `http://dark-serve-blazor-assets.dlio.localhost:11003/${name}`;
      }
    }).then(() => {
      DotNet.invokeMethod("Wasm", "InitializeDarkRuntime");
    });
  });
document.head.appendChild(blazorScript);
  onMounted(() => {
  fetch('/get-prompt')
  .then(response => response.text())
  .then(data => systemPromptValue.value = data);
});
</script>