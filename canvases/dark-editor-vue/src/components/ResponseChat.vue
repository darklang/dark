<template>
  <div>
    <div v-if="response">
      <p class="font-bold ">Response: {{ response }}</p>
    </div>
  </div>
</template>

<script>
export default {
  props: {
    response: {
      type: String,
      default: "",
    },
  },
};
</script>
