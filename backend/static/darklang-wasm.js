const Darklang = {
  _loadBlazorScript: async function () {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src =
        "http://dark-serve-static.dlio.localhost:11003/dark_wasm/_framework/blazor.webassembly.js";
      script.setAttribute("autostart", "false");
      script.addEventListener("load", resolve);
      script.addEventListener("error", reject);
      document.head.appendChild(script);
    });
  },

  init: async function () {
    await this._loadBlazorScript();

    await Blazor.start({
      loadBootResource: function (_type, name, _defaultUri, _integrity) {
        return `http://dark-serve-static.dlio.localhost:11003/dark_wasm/_framework/${name}`;
      },
    });

    let invoke = async function (method, ...args) {
      return await DotNet.invokeMethodAsync("Wasm", method, ...args);
    };

    await invoke("InitializeDarkRuntime");

    // return object to expose as 'darklang'
    return {
      editor: {
        loadClient: async function (url) {
          return await invoke("LoadClient", url);
        },
        handleEvent: async function (event) {
          return await invoke("HandleEvent", JSON.stringify(event));
        },
        // just for debugging purposes
        exportClient: async function () {
          return invoke("ExportClient");
        },
      },

      // just for dark-repl
      evalExpr: async function (serializedExpr) {
        return await invoke("EvalExpr", serializedExpr);
      },
    };
  },
};

window.Darklang = Darklang;
