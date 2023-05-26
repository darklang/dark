function setOutput(message) {
  document.getElementById("output-textarea").value = message;
}

(async () => {
  const darklang = await Darklang.init("dark-repl", setOutput);

  async function doEval() {
    const userCode = document.getElementById("input-textarea").value;
    darklang.handleEventRaw(userCode);
  }

  document
    .getElementById("input-textarea")
    .addEventListener("input", async () => {
      await doEval();
    });

  await doEval();
})();
