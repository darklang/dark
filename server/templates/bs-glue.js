var pageHidden = false;

window.Dark = {
  rollbar: {
    error: function (errorObj){ console.error(errorObj) }
  },
  analysis: {
    requestAnalysis : function (params) {
      console.log('request analysis');
      console.log(params);
    },
    receiveAnalysis : function (results) {
      var event = new CustomEvent('receiveAnalysis', {detail: results});
      document.dispatchEvent(event);
    }
  },
  ast: {
    positions: function (tlid) {
      var extractId = function (elem) {
        var className = elem.className;
        var matches = /.*id-([0-9]+).*/g.exec(className);
        var id = matches[1];
        
        if (typeof id === 'undefined')
          throw 'Dark.ast.atomPositions: Cannot match Blank(id) regex for '+className;
        
        var intID = parseInt(id);
        if(isNaN(intID))
          throw 'Dark.ast.atomPositions: Fail to parseInt '+id;

        return intID;
      };

      var find = function (tl, nested) {
        var atoms = [];
        tl.querySelectorAll(nested ? '.blankOr.nested' : '.blankOr:not(.nested)')
        .forEach((v,i,l) => {
          var rect = v.getBoundingClientRect();
          atoms.push({
            id: extractId(v),
            left: rect.left | 0,
            right: rect.right | 0,
            top: rect.top | 0,
            bottom: rect | 0
          });
        })
        return atoms;
      }

      var toplevels = document.getElementsByClassName('toplevel tl-'+tlid);
      
      if (toplevels.length == 0)
        throw 'Dark.ast.atomPositions: Cannot find toplevel: '+tlid;
      
      var tl = toplevels[0];
      
      return {
        atoms: find(tl, false),
        nested: find(tl, true)
      }
    }
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
    windowFocusChange(!hidden);
    pageHidden = hidden;
  }
}

setTimeout(function(){

  const params = JSON.stringify(
    {
      editorState: window.localStorage.getItem('editorState'),
      complete: complete,
      userContentHost: userContentHost,
      environment: environmentName
    });
  app = buckle.main(document.body, params);

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
