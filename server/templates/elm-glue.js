var analysisWorkerUrl = window.URL.createObjectURL(new Blob([document.querySelector('#analysisScript').textContent]));

const sendError = function (error, route, tlid){
  // send to rollbar
  Rollbar.error(
    error.str,
    error.obj,
    { route: route
      , tlid: tlid
    }
  );

  // log to console
  console.log(`Error processing analysis in (${route}): ${error.str}`);
  console.log(error.obj);

  // send to client
  elmapp.ports.displayError.send(
    `Error while executing (${route}): ${error}`);
};

window.Rollbar.configure(rollbarConfig);
elmapp = Elm.Main.fullscreen({
  editorState: window.localStorage.getItem('editorState'),
  complete: complete,
  userContentHost: userContentHost,
  environment: environmentName,
  csrfToken: csrfToken
});

elmapp.ports.setStorage.subscribe(function(editorState) {
  window.localStorage.setItem('editorState', editorState);
});

elmapp.ports.requestAnalysis.subscribe(function(params) {
  const bToString = (blankOr) => blankOr[2] || null;
  const spec = params.handler.spec;
  const route = `${bToString(spec.module)}, ${bToString(spec.name)}, ${bToString(spec.modifier)}`;

  if (window.Worker) {
    analysisWorker = new Worker(analysisWorkerUrl);
    analysisWorker.postMessage(
      { proto: window.location.protocol,
        params: JSON.stringify(params)
      }
    );

    analysisWorker.onmessage = function (e) {
      var result = e.data.analysis;
      var error = e.data.error;

      if (result && !error){
        elmapp.ports.receiveAnalysis.send(result);
      } else if (error) {
        sendError(error)
      }
    }
  } else {
    // do it without workers, it will be slow though
    Rollbar.error(
      'Web worker not available user browser.',
      navigator.userAgent,
      { route: route
        , tlid: params.handler.tlid
      }
    );
    elmapp.ports.displayError.send('Cannot perform analysis. Please use a browser that supports web workers.');
    // TODO provide link to list of browsers that support webworkers. Currently there's no easy way to do render injectable HTML in Elm.
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
