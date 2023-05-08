<script setup lang="ts">
import * as CodeMirror from 'codemirror'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/yonce.css'
import 'codemirror/mode/javascript/javascript.js'

import { ref, defineProps, defineEmits, onMounted } from 'vue'

import '../global.d.ts'

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
      let error = await response.text()
      throw new Error(error)
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
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="26"
              height="26"
              fill="#8f53ff"
              viewBox="0 0 24 24"
              id="play"
            >
              <path
                d="M8 6.82v10.36c0 .79.87 1.27 1.54.84l8.14-5.18c.62-.39.62-1.29 0-1.69L9.54 5.98C8.87 5.55 8 6.03 8 6.82z"
              ></path>
            </svg>
          </button>
          <button class="mb-3">
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="17"
              height="17"
              fill="#808080"
              viewBox="0 0 24 24"
              id="edit"
            >
              <path
                d="M18.2 24H3.8C1.7 24 0 22.3 0 20.2V5.8C0 3.7 1.7 2 3.8 2H11c.6 0 1 .4 1 1s-.4 1-1 1H3.8C2.8 4 2 4.8 2 5.8v14.3c0 1 .8 1.8 1.8 1.8h14.3c1 0 1.8-.8 1.8-1.8V13c0-.6.4-1 1-1s1 .4 1 1v7.2c.1 2.1-1.6 3.8-3.7 3.8z"
              ></path>
              <path
                d="M6 19c-.3 0-.5-.1-.7-.3-.2-.2-.3-.6-.3-.9l1-5c0-.2.1-.4.3-.5l12-12c.4-.4 1-.4 1.4 0l4 4c.4.4.4 1 0 1.4l-12 12c-.1.1-.3.2-.5.3l-5 1H6zm1.9-5.5l-.6 3.2 3.2-.6L21.6 5 19 2.4 7.9 13.5z"
              ></path>
            </svg>
          </button>
          <button @click="copyCode(responseIndex)" class="mb-3">
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="26"
              height="26"
              fill="#808080"
              viewBox="0 0 16 16"
              id="copy"
            >
              <path
                d="M4.00029246,4.08524952 L4,10.5 C4,11.8254834 5.03153594,12.9100387 6.33562431,12.9946823 L6.5,13 L10.9143985,13.000703 C10.7082819,13.5829319 10.1528467,14 9.5,14 L6,14 C4.34314575,14 3,12.6568542 3,11 L3,5.5 C3,4.84678131 3.41754351,4.29108512 4.00029246,4.08524952 Z M11.5,2 C12.3284271,2 13,2.67157288 13,3.5 L13,10.5 C13,11.3284271 12.3284271,12 11.5,12 L6.5,12 C5.67157288,12 5,11.3284271 5,10.5 L5,3.5 C5,2.67157288 5.67157288,2 6.5,2 L11.5,2 Z M11.5,3 L6.5,3 C6.22385763,3 6,3.22385763 6,3.5 L6,10.5 C6,10.7761424 6.22385763,11 6.5,11 L11.5,11 C11.7761424,11 12,10.7761424 12,10.5 L12,3.5 C12,3.22385763 11.7761424,3 11.5,3 Z"
              ></path>
            </svg>
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
