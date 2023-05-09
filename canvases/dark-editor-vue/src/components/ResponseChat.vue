<template>
  <div class="my-4 mx-auto max-w-2xl">
    <div class="flex items-start">
      <div
        class="relative inline-flex items-center justify-center w-10 h-10 overflow-hidden bg-[#a1b56c] rounded-full"
      >
        <span class="font-medium text-white">AI</span>
      </div>

      <div class="flex w-full bg-[#1B1B1B] ml-3 p-1.5 rounded-2xl overflow-auto">
        <div class="flex flex-col items-center mt-4 px-1">
          <button @click="executeCode(responseIndex)" class="mb-3">
            <fa icon="play" class="w-4 h-4 text-[#8f53ff]" />
          </button>
          <button class="mb-3">
            <fa icon="fa-pen-to-square" class="w-4 h-4 text-[#808080]" />
          </button>
          <button @click="copyCode(responseIndex)" class="mb-3">
            <fa icon="fa-clone" class="w-4 h-4 text-[#808080]" />
          </button>
        </div>

        <div class="ml-3 w-full p-5 border-l border-[#333333]">
          <textarea
            :value="response"
            :id="'editor-' + editorId"
            class="responseTextarea text-white bg-transparent w-full outline-none h-auto"
          ></textarea>
        </div>
      </div>
    </div>
    <form
      v-if="variables"
      @submit.prevent="submitForm"
      class="flex mt-2 justify-center"
    >
      <div v-for="(variable, index) in variables" :key="index">
        <label :for="'input-' + index" class="text-white ml-2">{{ variable }}</label>
        <input
          :id="'input-' + index"
          type="text"
          v-model="variableValues[index]"
          class="text-white bg-transparent my-2 ml-4 border-b border-white outline-none"
        />
      </div>
      <button
        v-if="variables && variables.length > 0"
        type="submit"
        class="bg-[#C56AE4] text-white m-2 px-2 py-1 rounded"
      >
        Submit
      </button>
    </form>
  </div>
</template>

<script setup lang="ts">
import { ref, defineProps, defineEmits, onMounted } from 'vue'
import '../global.d.ts'
import * as CodeMirror from 'codemirror'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/yonce.css'
import 'codemirror/mode/javascript/javascript.js'

const content = ref<string>('')
const editorId = ref<number>(0)

function updateEditorSize(editor: CodeMirror.Editor) {
  const lineHeight = editor.defaultTextHeight()
  const lineCount = editor.lineCount()
  const newHeight = lineCount * lineHeight + 5

  editor.setSize(null, newHeight)
}

onMounted(() => {
  const editorElement = document.getElementById(
    `editor-${editorId.value}`
  ) as HTMLTextAreaElement

  if (editorElement) {
    const editor = CodeMirror.fromTextArea(editorElement, {
      lineNumbers: true,
      mode: 'javascript',
      theme: 'yonce',
      viewportMargin: Infinity,
      value: props.response,
    })
    updateEditorSize(editor)

    editor.on('change', () => {
      content.value = editor.getValue()
      updateEditorSize(editor)
    })

    editorId.value = Math.floor(Math.random() * 1000) + 1
  }
})

const props = defineProps({
  response: {
    type: String,
    required: true,
  },
  responseIndex: {
    type: Number,
    required: true,
  },
})

const executeCode = async (index: number) => {
  let code: string = content.value

  try {
    const response = await fetch('/get-program-json', {
      method: 'POST',
      body: code,
    })

    if (!response.ok) {
      throw new Error('Error in parsing the expr and serializing it as JSON')
    }

    const userProgramJson = await response.text()
    const userProgramResult = await window.darklang.evalUserProgram(userProgramJson)

    console.log(userProgramResult) // TODO: something better than console.log
  } catch (error) {
    console.error('Error:', error)
  }
}

const copyCode = (index: number) => {
  let code: string = (
    document.querySelectorAll('.responseTextarea')[index] as HTMLInputElement
  ).value
  navigator.clipboard
    .writeText(code)
    .then(() => {})
    .catch((error) => {
      console.error('Error copying text to clipboard:', error)
    })
}

const variables = ref<string[]>([])
const variableValues = ref<string[]>([])
const emits = defineEmits(['message'])

let resp = props.response
const matchResult = resp.match(/variables:\n([\s\S]*)/)
const responseVariables = matchResult
  ? matchResult[1].trim().split('\n').filter(Boolean)
  : []
variables.value.push(...responseVariables)

const submitForm = () => {
  console.log(variableValues.value)
}
</script>
