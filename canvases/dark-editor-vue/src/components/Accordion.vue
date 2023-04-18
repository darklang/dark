
<template>
  <div class="text-white rounded-md flex flex-col stretch my-4 mx-auto max-w-2xl border border-green-300 shadow-lg shadow-black/10">
    <div @click="toggleAccordion()" class=" isOpen p-4 cursor-pointer flex items-center ">
      <slot name="accordion-header"></slot>
    </div>
    <div v-show="isOpen">
      <slot name="accordion-body"></slot>
    </div>
  </div>
</template>

<script lang="ts">
export default {
  data() {
    return {
      isOpen: false,
    };
  },
  methods: {
    toggleAccordion() {
      this.isOpen = !this.isOpen;
    },
  },
};
</script>