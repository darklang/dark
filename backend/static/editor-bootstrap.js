// Exposes a `Darklang.init(canvasName, callback)` function to bootstrap
// the Darklang runtime in WASM, in a WebWorker.

window.Darklang = {
  init: async function (canvasName, stateUpdatedCallback) {
    // time the below
    let startTime = performance.now();

    console.log("Initializing WASM Darklang runtime for editor");

    if (typeof stateUpdatedCallback !== "function") {
      throw new Error("usage: `Darklang.init(updatedState => { ... })`");
    }

    // create worker - this loads Blazor, and then Darklang stuff
    let resolveWorkerInit;
    const workerInitialized = new Promise((resolve, _reject) => {
      resolveWorkerInit = resolve;
    });
    const worker = new Worker("/static/dark-wasm-webworker.js");

    // receive messages (usually state updates) from worker
    worker.onmessage = e => {
      const message = e.data;

      if (typeof message !== "object") {
        console.error(
          "Unexpected non-object message received from worker",
          message,
        );
        return;
      }

      switch (message.type) {
        case "blazor-loaded":
          worker.postMessage({ type: "load-client", data: { canvasName } });
          break;

        case "client-loaded":
          resolveWorkerInit();
          break;

        case "eval-result": {
          const updatedState = message.data;
          stateUpdatedCallback(updatedState);
        }
      }
    };

    await workerInitialized;

    let endTime = performance.now();
    console.log(
      `Initialized WASM Darklang runtime for editor in ${
        Math.round((endTime - startTime) / 10) / 100
      }s`,
    );

    // return object to expose as 'darklang'
    return {
      /** Handle an event that the JS client has captured
       * and is forwarding to the WASM runtime */
      handleEvent: async function (event) {
        worker.postMessage({
          type: "handle-event",
          data: JSON.stringify(event),
        });
      },
      handleEventRaw: async function (event) {
        worker.postMessage({ type: "handle-event", data: event });
      },
    };
  },
};
