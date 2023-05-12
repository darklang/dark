<script setup lang="ts">
import type { Model } from '../types'

import Conversation from '../components/Conversation.vue'

let props = defineProps<{ state: Model }>()
</script>

<template>
  <main>
    <Conversation :state="props.state" />
  </main>
</template>
