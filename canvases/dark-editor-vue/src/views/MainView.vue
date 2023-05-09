<script setup lang="ts">

import PageHeader from '../components/common/PageHeader.vue'
import Accordion from '../components/common/Accordion.vue'
//import Conversation from '../components/Conversation.vue'

let props = defineProps({
  state: {
    type: Object, // todo
    required: true,
  },
})
</script>

<template>
  <main>
    <!-- Dark logo etc at the top -->
    <PageHeader />

    <!-- System prompt -->
    <Accordion class="mx-auto">
      <template v-slot:accordion-header>
        <span class="font-semibold text-xl">System prompt</span>
        <fa icon="fa-angle-down" class="w-4 h-4 ml-2" />
      </template>
      <template v-slot:accordion-body>
        <textarea
          class="bg-transparent outline-0 p-4 w-full resize-none h-96 max-h-96 overflow-y-auto border-0 text-white"
          ref="prompt"
          v-model="state.SystemPrompt"
        ></textarea>
      </template>
    </Accordion>

    <!-- Conversation -->
    <!-- <div>
      <Conversation :systemPromptValue="systemPromptValue" />
    </div> -->
  </main>
</template>