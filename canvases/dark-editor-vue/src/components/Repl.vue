<script setup lang="ts">
import { ref, onMounted } from 'vue'
import type { CodeSnippet } from '@/types'
import * as CodeMirror from 'codemirror'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/yonce.css'
import 'codemirror/mode/javascript/javascript.js'

const props = defineProps<{
  code: CodeSnippet
}>()

const textareaRef = ref(null) as any
const codeSnippet = ref('')
const content = ref<string>('')

function updateEditorSize(editor: CodeMirror.Editor) {
  const lineHeight = editor.defaultTextHeight()
  const lineCount = editor.lineCount()
  const newHeight = lineCount * lineHeight + 5
  editor.setSize(null, newHeight)
}

onMounted(() => {
  //@ts-ignore
  const editor = CodeMirror.fromTextArea(textareaRef.value, {
    lineNumbers: true,
    theme: 'yonce',
    mode: 'javascript',
    viewportMargin: Infinity,
    value: codeSnippet.value,
  })

  updateEditorSize(editor)

  editor.on('change', () => {
    content.value = editor.getValue()
    codeSnippet.value = content.value
    updateEditorSize(editor)
  })
})

async function runCode() {
  try {
    const textareaContent = textareaRef.value.value
    const evt = { CodeEval: [textareaContent] }
    const result = await window.darklang.handleEvent(evt)
    console.log('result', result)
  } catch (error) {
    console.error(error)
  }
}
</script>

<template>
  <div class="bg-[#1C1C1C]">
    <textarea
      v-model="codeSnippet"
      ref="textareaRef"
      class="text-white bg-transparent w-full outline-none h-auto"
      placeholder="Enter function here"
    ></textarea>
  </div>
  <button @click="runCode" class="bg-[#323232] text-white px-2 py-1 mt-2 rounded">
    Run
  </button>
  <div
    v-if="props.code.eval"
    class="text-xs p-2 bg-[#323232] rounded text-white mt-4"
  >
    {{ props.code.eval }}
  </div>
</template>
