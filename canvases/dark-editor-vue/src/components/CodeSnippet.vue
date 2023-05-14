<script setup lang="ts">
import { ref, onMounted } from 'vue'

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
      updateEditorSize(editor)
    })
  }
})
</script>

<template>
  <div class="p-4 m-6 bg-[#1C1C1C] rounded-2xl">
    <textarea
      v-model="codeSnippet"
      :id="`editor-${props.snippet.id}`"
      class="text-white bg-transparent w-full outline-none h-auto"
    ></textarea>
    <div class="flex justify-end mt-2">
      <button
        @click="runCode"
        class="px-4 py-2 text-xs font-bold text-white bg-transparent hover:bg-[#6756F6] border border-white/10 rounded"
      >
        eval
      </button>
    </div>
  </div>
  <div
    v-if="props.snippet.eval"
    class="text-xs p-2 bg-[#FEFEFE] rounded text-black mt-2"
  >
    {{ props.snippet.eval }}
  </div>
</template>
