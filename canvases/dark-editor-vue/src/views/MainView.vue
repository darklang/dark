<script setup lang="ts">
import { ref, defineProps } from 'vue'
import type { Model } from '../types'
import Function from '@/components/Function.vue'

const props = defineProps<{
  state: Model
}>()

let currentFunctions = ref([] as string[])
let clickedFunctions = new Set<string>()

function ShowFunction(functionItem: string) {
  if (!clickedFunctions.has(functionItem)) {
    clickedFunctions.add(functionItem)
    currentFunctions.value.push(functionItem)
  }
}
</script>

<template>
  <main>
    <div class="h-screen flex">
      <div class="min-h-screen bg-[#151515] text-white w-1/5">
        <h1 class="p-2 mt-2 font-semibold">Types and functions</h1>
        <div class="p-2 m-2 rounded bg-[#3a3a3a]">
          <h2 class="font-semibold">Types</h2>
          <ul class="text-[#9ea4ac] text-sm">
            <li>type-one</li>
            <li>type-two</li>
            <li>type-three</li>
          </ul>
        </div>
        <div class="p-2 m-2 rounded bg-[#3a3a3a]">
          <h2 class="font-semibold">Functions</h2>
          <ul>
            <li
              class="text-[#9ea4ac] text-sm cursor-pointer"
              v-for="(functionItem, index) in state.functions"
              :key="index"
              @click="ShowFunction(functionItem)"
            >
              {{ functionItem }}
            </li>
          </ul>
        </div>
        <div class="p-2 m-2 rounded bg-[#3a3a3a]">
          <h2 class="font-semibold">Handlers</h2>
          <ul class="text-[#9ea4ac] text-sm"></ul>
        </div>
      </div>

      <div class="flex flex-col h-full overflow-y-scroll pb-28">
        <div
          class="p-2 m-2 rounded bg-[#3a3a3a]"
          v-for="(currentFunction, index) in currentFunctions"
          :key="index"
        >
          <p class="text-[#eeeeee] text-sm">function-name-v</p>
          <Function v-model="currentFunctions[index]" />
        </div>
      </div>
    </div>
  </main>
</template>
