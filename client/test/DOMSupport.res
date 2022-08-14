// hmmm: does this do?

let init = () => {
  ignore(
    %raw(`
  function () {
    require('jsdom-global')();
    var Storage = require('dom-storage');
    global.sessionStorage = new Storage(null, { strict: false });
  } ()
  `),
  )
  ()
}
