<template>
  <div>
    <Prompt @prompt-submitted="submitPrompt" />
    <div v-for="(conversation, index) in conversations" :key="index">
      <UserChat :messages="conversation.userMessages" />
      <ResponseChat :response="conversation.response" />
    </div>
  </div>
</template>

<script>
import Prompt from "./components/Prompt.vue";
import UserChat from "./components/UserChat.vue";
import ResponseChat from "./components/ResponseChat.vue";
// import { sendMessage } from "./worker-api"

export default {
  components: {
    Prompt,
    UserChat,
    ResponseChat,
  },
  data() {
    return {
      conversations: [],
    };
  },
  methods: {
    // postMessage(){
    //   worker.sendMessage(this.message)
    // },

    async submitPrompt(prompt) {
      const conversation = {
        userMessages: [prompt],
        response: "",
      };
      this.conversations.push(conversation);

      // Replace this with your own API call to get a response from OpenAI API
      const response = "Success! Example response from OpenAI API";
      conversation.response = response;
    },
  },
};
</script>
