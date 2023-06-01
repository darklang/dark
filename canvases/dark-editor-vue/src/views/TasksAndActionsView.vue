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
    <h1>Tasks & Actions</h1>
    <h2>Tasks</h2>
    <ul>
      <li v-for="(task, index) in tasks" :key="index">
        {{ task.description }}
      </li>
    </ul>

    <h2>Actions</h2>
    <ul>
      <li v-for="(action, index) in actions" :key="index">
        {{ action.description }}
      </li>
    </ul>
  </div>
</template>
