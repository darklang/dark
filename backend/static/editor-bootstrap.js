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

  init: async function (sourceURL, parseUrl) {
    if (!sourceURL || !parseUrl) {
      throw new Error(
        "Darklang.init() requires a sourceURL and parseUrl to be passed in"
      );
    }

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

    await invoke("Test");

    //await invoke("LoadClient", sourceURL, parseUrl);

    // return object to expose as 'darklang'
    return {
      /** Handle an event that the JS client has captured
       * and is forwarding to the WASM runtime */
      handleEvent: async function (event) {
        this.handleEventRaw(JSON.stringify(event));
      },
      handleEventRaw: async function (event) {
        return await invoke("HandleEvent", event);
      },
    };
  },
};

window.Darklang = Darklang;
