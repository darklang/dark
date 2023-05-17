<script setup lang="ts">
import type { Model } from '../types'

import Conversation from '../components/Conversation.vue'
import Header from '../components/Header.vue'

let props = defineProps<{ state: Model }>()
</script>

<template>
  <main>
    <Header />
    <Conversation :state="props.state" />
  </main>
</template>
