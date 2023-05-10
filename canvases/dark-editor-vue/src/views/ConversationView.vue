<script setup lang="ts">

import Conversation from '../components/Conversation.vue'

let props = defineProps({
  state: {
    type: Object, // todo
    required: true,
  },
})
</script>

<template>
  <main>
    <Conversation v-bind:state=state />
  </main>
</template>
