window.Rollbar.configure(rollbarConfig);
elmapp = Elm.Main.fullscreen({
  editorState: window.localStorage.getItem('editorState'),
  complete: complete,
  userContentHost: userContentHost,
  environment: environmentName
});

elmapp.ports.setStorage.subscribe(function(editorState) {
  window.localStorage.setItem('editorState', editorState);
});

elmapp.ports.requestAnalysis.subscribe(function(params) {
  function bToString (blankOr) { return blankOr[2] || null; }
  const spec = params.handler.spec;
  const route =
    `${bToString(spec.module)}, ${bToString(spec.name)}, ${bToString(spec.modifier)}`;
  try {
    <!-- console.log("Performing analysis for: " + route); -->
    <!-- console.log(params); -->
    var result = darkAnalysis.performAnalysis(JSON.stringify(params));
    <!-- console.log(JSON.parse(result)); -->
    elmapp.ports.receiveAnalysis.send(result);
  }
  catch (error) {
    var errorName = null;
    var errorMsg = null;
    var errString = null;

    try {
      // OCaml errors come in a few constructs - get the data we can
      try { errorName = error[1][1].c; } catch (_) {}
      try { errorMsg = error[2][1].c; } catch (_) {}
      try { errorMsg = error[2].c; } catch (_) {}
      errString = `${errorName} - ${errorMsg}`;
    } catch (_) { }

    // send to rollbar
    window.Rollbar.error(errString, error, { route: route
                                          , tlid: params.handler.tlid});

    // log to console
    console.log(`Error processing analysis in (${route}): ${errString}`);
    if (!errorName || !errorMsg) {
      console.log(error);
    }

    // send to client
    elmapp.ports.displayError.send(
      `Error while executing (${route}): ${errString}`);
  }
})

elmapp.ports.sendRollbar.subscribe(function(obj) {
  window.Rollbar.error(obj.message, obj.url, null, null, obj.custom);
});

window.onerror = function (msg, url, line, col, error) {
  window.Rollbar.error(msg, error);
  elmapp.ports.displayError.send(msg);
};

bundle.mousewheel(function(dx,dy, dz, ev){
  // NB: rounding values here because Elm wants ints
  elmapp.ports.mousewheel.send([Math.round(dx),Math.round(dy)]);
});

