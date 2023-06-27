<script setup lang="ts">
import { ref, defineProps } from 'vue'
import type { Model } from '../types'
import Function from '@/components/Function.vue'
import Repl from '@/components/Repl.vue'

const props = defineProps<{
  state: Model
}>()

let functionsList = ref(new Set<string>())
let typesList = ref(new Set<string>())

function getFunctions(functionItem: string) {
  if (!functionsList.value.has(functionItem)) {
    functionsList.value.add(functionItem)
  }
}

function getTypes(typeItem: string) {
  if (!typesList.value.has(typeItem)) {
    typesList.value.add(typeItem)
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
            <li
              class="text-[#9ea4ac] text-sm cursor-pointer"
              v-for="(typeItem, index) in state.types"
              :key="index"
              @click="getTypes(typeItem)"
            >
              {{ typeItem }}
            </li>
          </ul>
        </div>
        <div class="p-2 m-2 rounded bg-[#3a3a3a]">
          <h2 class="font-semibold">Functions</h2>
          <ul>
            <li
              class="text-[#9ea4ac] text-sm cursor-pointer"
              v-for="(functionItem, index) in state.functions"
              :key="index"
              @click="getFunctions(functionItem)"
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
          v-for="(currentFunction, index) in functionsList"
          :key="index"
        >
          <p class="text-[#eeeeee] text-sm">function-name-v</p>
          <Function :modelValue="currentFunction" />
        </div>

        <div
          class="p-2 m-2 rounded bg-[#3a3a3a]"
          v-for="(currentType, index) in typesList"
          :key="index"
        >
          <p class="text-[#eeeeee] text-sm">type-name</p>
          <Function :modelValue="currentType" />
        </div>
      </div>

      <div>
        <Repl :snippet="state.codeSnippets[0]" />
      </div>
    </div>
  </main>
</template>
