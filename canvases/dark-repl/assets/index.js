function setOutput(message) {
  document.getElementById("output-textarea").value = message;
}

(async () => {
  const darklang = await Darklang.init();

  await darklang.loadClient(
    "http://dark-repl.dlio.localhost:11003/assets/client.dark",
    "http://dark-repl.dlio.localhost:11003/get-program-json",
  );

  async function doEval() {
    try {
      const input = document.getElementById("input-textarea").value;
      const result = await darklang.handleEventRaw(input);
      setOutput(result);
    } catch (error) {
      //setOutput("Error" + error);
      console.warn("Couldn't parse", error);
    }
  }

  document
    .getElementById("input-textarea")
    .addEventListener("input", async () => {
      await doEval();
    });

  await doEval();
})();
