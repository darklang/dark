// "use strict";

// This file based on driveby.js from alltonp/driveby
var server = require('webserver');
var fs = require('fs');
var webpage = require('webpage');

var numberOfBrowsers = 4;
var nextPort = 9000;
var screenshotAllSteps = false;
var screenshotFailures = true;

var started = new Date().getTime();
var urls = ["http://test_empty_integration_test.localhost:8000/admin/integration_test"];
var pages = [];
var stubs = {};

function error(msg) {
  console.log(msg);
  phantom.exit();
}

// TODO: parallelize
function run() {
  var p = webpage.create();
  p.viewportSize = {
    width: 1950,
    height: 1350
  };

  p.onConsoleMessage = function(msg, lineNum, sourceId) {
    console.log('CONSOLE: [' + msg + '] (from line #' + lineNum + ' in "' + sourceId + '")');
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

  p.open(urls[0], function (status) {
    if (status !== 'success') { error("couldn't open url: " + status); }
    console.log("opened url");

    waitFor(p, isLoaded, function () {
      console.log("isLoaded");
      enterChangesState(p);
    })

  });
}

function isLoaded(p) {
  return isUniqueInteractable (p, "#darkErrors");
}

function isThere(n) {
  var i = 0;
  return function(p) {
    i++;
    p.render("lol" + i + ".png");
    isUniqueInteractable(p, n);
  }
}

function enterChangesState(p) {
  p.sendEvent('keypress', p.event.key.Enter);

 var a = p.evaluate(function() {
    return document.getElementById("finishIntegrationTest").getBoundingClientRect();
  });

  console.log(a.top);
  console.log(a.bottom);
  console.log(a.left);
  console.log(a.right);

  p.sendEvent("click", a.left + 5, a.top + 5);
  waitFor(p, testFinished, endTest);
}

function testFinished(p) {
  return p.evaluate(function() {
    var signal = document.getElementById("integrationTestSignal");
    return (signal !== null);
  })
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
function waitFor(page, testFn, onReady) {
  console.log("waiting");
  var maxtimeOutMillis = 3000,
      start = new Date().getTime(),
      condition = false,
      interval = setInterval(function() {
        if ( (new Date().getTime() - start < maxtimeOutMillis) && !condition ) {
          condition = testFn(page);
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


//TIP: http://stackoverflow.com/questions/15739263/phantomjs-click-an-element
function click(page, context, selector) {
  waitFor(page, context, function() { return isUniqueInteractable(page, selector); }
    , function() { page.evaluate(function(theSelector) { document.querySelector(theSelector).click(); }, selector); }
    , function() { return describeFailure(page, selector); }
  );
}

function enter(page, context, selector, value) {
  waitFor(page, context, function() { return isUniqueInteractable(page, selector); }
    , function() { //action
        page.evaluate(function(theSelector, theValue) { document.querySelector(theSelector).focus(); }, selector, value);
        page.sendEvent('keypress', value); }
    , function() { return describeFailure(page, selector); }
  );
}

function isUniqueInteractable(page, selector) {
  return page.evaluate(
    function(theSelector) {
      console.log("checking selector " + theSelector);
      var e = document.querySelectorAll(theSelector)
      console.log("found " + e.length);
      // stackoverflow.com/questions/19669786/check-if-element-is-visible-in-dom
      return e.length == 1 && !!( e[0].offsetWidth || e[0].offsetHeight || e[0].getClientRects().length ); // aka visible
    }, selector);
}

function describeFailure(page, selector) {
  return page.evaluate(function(theSelector) {
    return "expected 1 element for " + theSelector + " found " + document.querySelectorAll(theSelector).length;
  }, selector);
}

run();
