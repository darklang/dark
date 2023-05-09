<template>
  <div>
    <div class="h-[542px] overflow-y-scroll pb-16" v-if="prompts.length">
      <div v-for="(prompt, index) in prompts" :key="index">
        <UserChat :promptValue="prompt" />
        <ResponseChat
          @message="handleMessage"
          :response="responses[index]"
          :responseIndex="index"
          v-if="responses[index]"
        />
      </div>
      <Result :message="message" v-if="message" />
    </div>

    <div class="absolute bottom-0 left-0 w-full pt-2 bg-[#151515]">
      <form
        @submit.prevent="submitPrompt"
        class="flex flex-row stretch my-4 mx-auto max-w-2xl"
      >
        <div class="relative flex h-full flex-1">
          <div
            class="w-full relative flex flex-col flex-grow py-3 pl-4 border border-white/10 rounded-md shadow-black/10"
          >
            <textarea
              ref="content"
              autosize
              v-model="prompt"
              aria-multiline="true"
              rows="1"
              placeholder="What are you building today?"
              class="w-full h-6 max-h-44 outline-none m-0 resize-none overflow-y-auto border-0 bg-transparent text-white py-0 pl-2 pr-11"
            ></textarea>
            <button
              type="submit"
              class="absolute bottom-2 right-2 py-1 px-2 mx-2 rounded-md text-white bg-[#C56AE4] hover:bg-[#9f56b8]"
              :class="{ 'opacity-50 cursor-not-allowed': isLoading }"
              :disabled="isLoading"
            >
              <span v-if="!isLoading">send</span>
              <span v-else class="flex">
                <fa icon="fa-spinner" class="animate-spin w-6 p-1" />
              </span>
            </button>
          </div>
        </div>
      </form>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'
import UserChat from './UserChat.vue'
import ResponseChat from './ResponseChat.vue'
import autosize from 'autosize'
import Result from './Result.vue'

const prompts = ref<string[]>([])
const prompt = ref('')
const responses = ref<string[]>([])
const message = ref('')
const isLoading = ref(false)

const textarea = document.querySelector('textarea')
if (textarea !== null) {
  autosize(textarea)
}
const content = ref()
onMounted(() => autosize(content.value))

const props = defineProps({
  systemPromptValue: {
    type: String,
    default: '',
  },
})

const handleMessage = (value: string) => {
  message.value = value
}

const submitPrompt = async () => {
  prompts.value.push(prompt.value)

  try {
    isLoading.value = true
    const response = await fetch('/api/gpt4', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        prompt: props.systemPromptValue + ' ' + prompt.value,
      }),
    })

    if (!response.ok) {
      console.error('Error sending prompt to server')
      return
    }

    prompt.value = ''
    const data = await response.json()
    responses.value.push(data.choices[0].text)
  } catch (error) {
    console.error('Error sending prompt to server:', error)
  }

  // test ui without using tokens
  // const data = '(let a = 1 \n a)'
  // responses.value.push(data)

  isLoading.value = false
  //reset prompt textarea size
  autosize.destroy(content.value)
  autosize(content.value)
}
</script>
