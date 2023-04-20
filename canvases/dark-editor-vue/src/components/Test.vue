<!-- TODO: Remove this is jut to test wasm in vue-->
<template>
  <div class="container">
    <div class="left-side">
      <textarea
        id="input-textarea"
        class="textarea"
        v-model="inputText"
        ref="inputTextarea"
      ></textarea>
      <button class="submit-button" @click="handleSubmit">Submit</button>
    </div>
    <div class="right-side">
      <textarea
        id="output-textarea"
        class="textarea"
        v-model="outputText"
        disabled
      ></textarea>
    </div>
  </div>
</template>

<script>
  const blazorScript = document.createElement("script");
  blazorScript.setAttribute(
    "src",
    "http://dark-serve-blazor-assets.dlio.localhost:11003/blazor.webassembly.js",
  );
  blazorScript.setAttribute("autostart", "false");
  blazorScript.addEventListener('load', async () => {
  await Blazor.start({
    loadBootResource: function (type, name, defaultUri, integrity) {
  return `http://dark-serve-blazor-assets.dlio.localhost:11003/${name}`;
        }
      }).then(() => {
        DotNet.invokeMethod("Wasm", "InitializeDarkRuntime");
      });
    });
    document.head.appendChild(blazorScript);
window.handleDarkResult = function (message) {
  console.log("handleDarkResult", message);
  this.outputText = message;
};
export default {
  data() {
    return {
      inputText: "(let a = 1 + 2\nList.repeat 10 a\n)",
      outputText: "",
    };
  },
  methods: {
    async handleSubmit() {
      try {
        const response = await fetch("/get-expr-json", {
          method: "POST",
          body: this.inputText,
        });
        if (!response.ok) {
          throw new Error(
            "Error in parsing the expr and serializing it as JSON"
          );
        }
        const exprJson = await response.text();
        DotNet.invokeMethod("Wasm", "LoadExpr", exprJson);
        DotNet.invokeMethod("Wasm", "EvalExpr");
      } catch (error) {
        this.outputText = "Error" + error;
        console.error("Error:", error);
      }
    },
  },
};
</script>
