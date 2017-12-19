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
      console.log("waitfor successfor");
      p.close();
      console.log("Done " + (new Date().getTime() - started) + "ms.");
      phantom.exit();
    })
  });
}

function isLoaded(p) {
  return isUniqueInteractable (p, "#darkErrors");
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
            phantom.exit(1);
          } else {
            onReady();
            clearInterval(interval);
          }
        }
      }, 250);
};

// function respond(page, context, failures) {
//   var y = Date.now()
//   var x = y.toString()
// //  console.log(x)
//   //TODO: just need a stayOpenOnFailure
//   var screenshot = page != null && (screenshotAllSteps || (screenshotFailures && failures.length > 0) )
//   if (screenshot) page.render(started + '/' + context.scriptId + '/' + context.stepId + '.png')
// //  if (screenshot) console.log(page.plainText)
// }
//

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

function assert(page, context, description, selector, condition, expected) {
  if (condition == "textContains") { return assertCondition(page, context, selector, expected, description,
    function(e, theExpected) { return e.length == 1 && e[0].textContent.indexOf(theExpected) >= 0; });
  }
  else if (condition == "textEquals") { return assertCondition(page, context, selector, expected, description,
    function(e, theExpected) { return e.length == 1 && e[0].textContent == theExpected; });
  }
  else { respond(page, context, [ "sorry, I don't know how to assert condition: " + JSON.stringify(condition) ]); }
}

function assertCondition(page, context, selector, expected, description, conditionFunc) {
  waitFor(page, context, function() { //condition
      return page.evaluate(function(theSelector, theExpected, theDescription, theConditionFunc) {
        return theConditionFunc(document.querySelectorAll(theSelector), theExpected);
      }, selector, expected, description, conditionFunc); }
    , function() { } //action
    , function() { //failure
        return page.evaluate(function(theSelector, theDescription) {
          var e = document.querySelectorAll(theSelector);
          if (e.length != 1) { return "expected 1 element for " + theSelector + " found " + e.length; }
          else { return "expected " + theDescription + " for '" + e[0].textContent + "'"; }
        }, selector, description); }
  );
}

function isUniqueInteractable(page, selector) {
  return page.evaluate(
    function(theSelector) {
      console.log("checking selector " + theSelector);
      var e = document.querySelectorAll(theSelector)
      console.log("found " + e);
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
