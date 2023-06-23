<script setup lang="ts">
import { ref, defineProps, defineEmits, onMounted } from 'vue'
import * as CodeMirror from 'codemirror'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/yonce.css'
import 'codemirror/mode/javascript/javascript.js'

const props = defineProps({
  modelValue: String,
})

const emits = defineEmits(['update:modelValue'])

let textarea = ref(null) as any
let editor = null as any

function updateEditorSize(editor: CodeMirror.Editor) {
  const lineHeight = editor.defaultTextHeight()
  const lineCount = editor.lineCount()
  const newHeight = lineCount * lineHeight + 5

  editor.setSize(null, newHeight)
}

onMounted(() => {
  editor = CodeMirror.fromTextArea(textarea.value, {
    lineNumbers: true,
    theme: 'yonce',
    mode: 'javascript',
    viewportMargin: Infinity,
  })

  editor.setValue(props.modelValue)
  updateEditorSize(editor)

  editor.on('changes', () => {
    const value = editor.getValue()
    emits('update:modelValue', value)
    updateEditorSize(editor)
  })
})
</script>

<template>
  <div class="rounded">
    <textarea
      ref="textarea"
      class="text-white bg-transparent w-full outline-none h-auto"
      placeholder="Enter function here"
    ></textarea>
  </div>
</template>
