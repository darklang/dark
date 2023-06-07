<script setup lang="ts">
import { defineProps } from 'vue'
import type { Task, Action } from '@/types'

const props = defineProps<{
  tasks: Task[]
  actions: Action[]
}>()
</script>

<template>
  <div class="min-h-screen bg-[#151515] text-white">
    <h1 class="p-2 mt-2 font-semibold">Tasks And Actions</h1>
    <div class="p-2 m-2 rounded bg-[#3a3a3a]">
      <h2 class="font-semibold">Tasks</h2>
      <ul class="text-[#9ea4ac] text-sm">
        <li v-for="(task, index) in tasks" :key="index">
          {{ task.description }}
        </li>
      </ul>
    </div>
    <div class="p-2 m-2 rounded bg-[#3a3a3a]">
      <h2 class="font-semibold">Actions</h2>
      <ul class="text-[#9ea4ac] text-sm">
        <li v-for="(action, index) in actions" :key="index">
          {{ action.description }}
        </li>
      </ul>
    </div>
  </div>
</template>
