<template>
  <div>
    <div v-for="(message, index) in messages" :key="index">
      <p>Your messages: {{ message }}</p>
    </div>
  </div>
</template>

<script>
export default {
  props: {
    messages: {
      type: Array,
      default: () => [],
    },
  },
};
</script>
