<template>
  <div>
    <input v-model="prompt" placeholder="Enter your prompt" />
    <button @click="submitPrompt">Submit</button>
  </div>
</template>

<script>
export default {
  data() {
    return {
      prompt: "",
    };
  },
  methods: {
    submitPrompt() {
      this.$emit("prompt-submitted", this.prompt);
      this.prompt = "";
    },
  },
};
</script>
