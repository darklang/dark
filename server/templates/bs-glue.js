var pageHidden = false;

window.Rollbar = {
  error: (errorObj) => console.error(errorObj)
}

window.darkAnalysis = {
  requestAnalysis : (params) => { console.log('request analysis'); console.log(params); },
  receiveAnalysis : (results) => {
    var event = new CustomEvent('receiveAnalysis', {detail: results});
    document.dispatchEvent(event);
  }
}

function triggerAnalysis (){
  var res = { liveValues: "stuff", availableVarnames: ["a", "b", "c"] };
  darkAnalysis.receiveAnalysis(res);
}

function displayError (msg){
  var event = new CustomEvent('displayError', {detail: msg});
  document.dispatchEvent(event);
}

function windowFocusChange (visible){
  console.log('window focus changed '+visible);
  var event = new CustomEvent('windowFocusChange', {detail: visible});
  document.dispatchEvent(event);
}

function visibilityCheck(){
  var hidden = false;
  if (typeof document.hidden !== 'undefined') {
    hidden = document.hidden;
  } else if (typeof document.mozHidden !== 'undefined') {
    hidden = document.mozHidden;
  } else if (typeof document.msHidden !== 'undefined') {
    hidden = document.msHidden;
  } else if (typeof document.webkitHidden !== 'undefined') {
    hidden = document.webkitHidden;
  }

  if (pageHidden != hidden) {
    windowFocusChange(hidden);
    pageHidden = hidden;
  }
}

setTimeout(function(){
  app = buckle.main(document.body);

  window.onresize = function(evt){
    const size = {
      width : window.innerWidth,
      height: window.innerHeight
    }
    var event = new CustomEvent('windowResize',
      { detail : size })
    document.dispatchEvent(event)
  };

  window.onfocus = function(evt){ windowFocusChange(true) };
  window.onblur = function(evt){ windowFocusChange(false) };
  setInterval(visibilityCheck, 2000);

}, 1)
