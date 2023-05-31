<script setup lang="ts">
import { ref, onMounted, watch } from 'vue'

import type { CodeSnippet } from '@/types'
import * as CodeMirror from 'codemirror'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/yonce.css'
import 'codemirror/mode/javascript/javascript.js'

const content = ref<string>('')

function updateEditorSize(editor: CodeMirror.Editor) {
  const lineHeight = editor.defaultTextHeight()
  const lineCount = editor.lineCount()
  const newHeight = lineCount * lineHeight + 5

  editor.setSize(null, newHeight)
}

const props = defineProps<{
  snippet: CodeSnippet
}>()

const codeSnippet = ref(props.snippet.code)

async function runCode() {
  try {
    const evt = { UserRequestedCodeEval: [props.snippet.id, codeSnippet.value] }
    const result = await window.darklang.handleEvent(evt)
    console.log('result', result)
  } catch (error) {
    console.error(error)
  }
}

onMounted(() => {
  const editorElement = document.getElementById(
    `editor-${props.snippet.id}`
  ) as HTMLTextAreaElement

  if (editorElement) {
    const editor = CodeMirror.fromTextArea(editorElement, {
      lineNumbers: true,
      mode: 'javascript',
      theme: 'yonce',
      viewportMargin: Infinity,
      value: codeSnippet.value,
    })
    updateEditorSize(editor)

    editor.on('change', () => {
      content.value = editor.getValue()
      codeSnippet.value = content.value
      updateEditorSize(editor)
    })
  }
})
</script>

<template>
  <div class="flex p-4 m-2 mt-6 bg-[#1C1C1C] rounded-2xl overflow-hidden relative">
    <div class="flex flex-col items-center mt-4 px-1">
      <button class="mb-3" @click="runCode">
        <fa icon="play" class="w-4 h-4 text-[#8f53ff]" />
      </button>
      <button class="mb-3">
        <fa icon="fa-pen-to-square" class="w-4 h-4 text-[#808080]" />
      </button>
      <button class="mb-3">
        <fa icon="fa-clone" class="w-4 h-4 text-[#808080]" />
      </button>
    </div>
    <div class="ml-3 border-l border-[#333333] overflow-scroll">
      <textarea
        v-model="codeSnippet"
        :id="`editor-${props.snippet.id}`"
        class="text-white bg-transparent w-full outline-none h-auto absolute inset-1"
      ></textarea>
    </div>
  </div>
  <div
    v-if="props.snippet.eval"
    class="text-xs p-2 bg-[#323232] rounded text-white mt-4"
  >
    {{ props.snippet.eval }}
  </div>
</template>
