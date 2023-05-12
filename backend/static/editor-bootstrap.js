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
      /** Load a client Dark code from a URL */
      loadClient: async function (sourceURL, parseUrl) {
        return await invoke("LoadClient", sourceURL, parseUrl);
      },

      /** Handle an event that the JS client has captured
       * and is forwarding to the WASM runtime */
      handleEventRaw: async function (event) {
        return await invoke("HandleEvent", event);
      },
      handleEvent: async function (event) {
        return await invoke("HandleEvent", JSON.stringify(event));
      },
    };
  },
};

window.Darklang = Darklang;
