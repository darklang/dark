<template>
  <textarea ref="content" class="text-white bg-transparent w-full outline-none">{{ $props.response }}</textarea>
</template>

<script setup>
import { ref, onMounted} from "vue";
import autosize from "autosize";

const content = ref("");
onMounted(() => autosize(content.value));
</script>
