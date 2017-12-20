var webpage = require('webpage');
var system = require('system');

var testname = system.args[1];
var filename = testname + ".js"
var url = "http://test_" + testname + ".localhost:8000/admin/integration_test"

if (!phantom.injectJs(filename)) {
  console.log("failed to open testfile:" + filename);
  phantom.exit(1);
};




// This file based on driveby.js from alltonp/driveby
function run() {
  console.log("running test: " + testname);

  var p = webpage.create();

  // Use a big screen or everything overlaps.
  p.viewportSize = {
    width: 1950,
    height: 1350
  };

  p.onConsoleMessage = function(msg, lineNum, sourceId) {
    if (lineNum && sourceId) {
      console.log('CONSOLE: ' + msg + ' (from line #' + lineNum + ' in "' + sourceId + '")');
    } else {
      console.log('CONSOLE: ' + msg);
    }
  };

  p.onError = function(msg, trace) {
    var msgStack = ['JS EXCEPTION: ' + msg];
    if (trace && trace.length) {
      msgStack.push('TRACE:');
      trace.forEach(function(t) {
        msgStack.push(' -> ' + (t.file || t.sourceURL) + ': ' + t.line + (t.function ? ' (in function ' + t.function +')' : ''));
      });
    }
    console.error(msgStack.join('\n'));
    phantom.exit(1);
  };

  // test starts here
  p.open(url, function (status) {
    if (status !== 'success') { error("couldn't open url: " + status); }

    waitFor(p, "#darkErrors", testFn);
  });
}

function clickFinishTest(p) {
  var a = p.evaluate(function() {
    return document.getElementById("finishIntegrationTest").getBoundingClientRect();
  });

  p.sendEvent("click", a.left + 5, a.top + 5);
  waitFor(p, "#integrationTestSignal", endTest);
}

function endTest(p) {
  var wasSuccessful = p.evaluate(function() {
    var signal = document.getElementById("integrationTestSignal");
    return (signal !== null) ? signal.classList.contains('success') : false;
  });

  p.render("testEnded.png");

  if (wasSuccessful) {
    console.log("Test successful")
    phantom.exit(0);
  } else {
    console.log("Test failed")
    phantom.exit(1);
  }
}


// shamelessly stolen from: https://github.com/ariya/phantomjs/blob/master/examples/waitfor.js
function waitFor(page, selector, onReady) {
  var maxtimeOutMillis = 3000,
      start = new Date().getTime(),
      condition = false,
      interval = setInterval(function() {
        if ( (new Date().getTime() - start < maxtimeOutMillis) && !condition ) {
          condition = isUniqueInteractable(page, selector);
        }
        else {
          if (!condition) {
            onReady(page)
          } else {
            onReady(page);
            clearInterval(interval);
          }
        }
      }, 250);
};


function isUniqueInteractable(page, selector) {
  return page.evaluate(
    function(theSelector) {
      console.log("checking for " + theSelector);
      var e = document.querySelectorAll(theSelector)
      console.log("found");
      // stackoverflow.com/questions/19669786/check-if-element-is-visible-in-dom
      return e.length == 1 && !!( e[0].offsetWidth || e[0].offsetHeight || e[0].getClientRects().length ); // aka visible
    }, selector);
}

run();
