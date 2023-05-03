const Darklang = {
  _loadBlazorScript: async function () {
    return new Promise((resolve, reject) => {
      const script = document.createElement('script');
      script.src = 'http://dark-serve-static.dlio.localhost:11003/dark_wasm/_framework/blazor.webassembly.js';
      script.setAttribute('autostart', 'false');
      script.addEventListener('load', resolve);
      script.addEventListener('error', reject);
      document.head.appendChild(script);
    });
  },

  init: async function() {
    await this._loadBlazorScript()

    await Blazor.start({
      loadBootResource: function(_type, name, _defaultUri, _integrity) {
        return `http://dark-serve-static.dlio.localhost:11003/dark_wasm/_framework/${name}`;
      }
    });

    await DotNet.invokeMethodAsync("Wasm", "InitializeDarkRuntime");

    // return object to expose as 'darklang'
    return {
      evalExpr: async function(serializedExpr) {
        return await DotNet.invokeMethodAsync("Wasm", "EvalExpr", serializedExpr);
      },
      evalExprAndReturnResult: async function(serializedExpr) {
        return await DotNet.invokeMethodAsync("Wasm", "EvalExprAndReturnResult", serializedExpr);
      },
    };
  }
};

window.Darklang = Darklang;