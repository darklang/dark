var server = require('webserver');
var fs = require('fs');
var webpage = require('webpage')

//TODO: put this in arg[] to this script .. see https://github.com/ariya/phantomjs/blob/master/examples/arguments.js
//TODO: or loading it from a file (and creating if doesnot exist) driveby.json
var numberOfBrowsers = 4;
var nextPort = 9000;
var surpressPageErrors = true;
var screenshotAllSteps = false;
var screenshotFailures = true;

var started = new Date().getTime();
var pages = [];
var stubs = {};

for (var i = 0; i < numberOfBrowsers; i+=1) {
  var p = webpage.create();

  //TODO: make this a config option - surpressCommandLogging
  p.onConsoleMessage = function(msg, lineNum, sourceId) {
    console.log('CONSOLE: [' + msg + '] (from line #' + lineNum + ' in "' + sourceId + '")');
  };

  if (surpressPageErrors) { p.onError = function(msg, trace) {}; }

  pages.push(p);
}

//shamelessly stolen from: https://github.com/ariya/phantomjs/blob/master/examples/waitfor.js
"use strict";
//TODO: should screenshot be before the action - might be more useful for debug
//TODO: might be nice highlight the interactable element (like watir) before we do the action ...
//TODO: do as much as possible in elm .. e.g. build the test report in elm, save it in js
//TODO: this script should have a return value of success of failure, for scripts to use ...
//TODO: rename functions and condition to be more readable
//TODO: consider running this as a daemon
//TODO: this waiting could be in elm ... would possibly need to subscribe to time
//TODO: changes to this file should also trigger autotest.sh
function waitFor(page, context, testFx, onReady, onFail, timeOutMillis) {
  var maxtimeOutMillis = timeOutMillis ? timeOutMillis : 3000, //TODO: make this a config option
      start = new Date().getTime(),
      condition = false,
      interval = setInterval(function() {
        if ( (new Date().getTime() - start < maxtimeOutMillis) && !condition ) { condition = testFx(); }
        else {
          if (!condition) { clearInterval(interval); respond(page, context, [ onFail() ]); }
          else { onReady(); clearInterval(interval); respond(page, context, []); }
        }
      }, 1); //TODO: make this a config option
};

//TODO: make this an argv ... maybe support multiple file inputs .... run if successful ... good for autotesting
phantom.injectJs("tests.js") ? "... done injecting tests.js!" : "... failed injecting tests.js!";

var flags = { numberOfBrowsers: pages.length };
var app = Elm.DrivebyTest.embed(document.createElement('div'), flags);

app.ports.requests.subscribe(function(request) {
  var command = request.step.command
  var name = command.name
  var context = request.context
  var page = pages[context.browserId]

  if (name == "click") { click(page, context, command.args[0]); }
  else if (name == "enter") { enter(page, context, command.args[0], command.args[1]); }
  else if (name == "goto") { goto(page, context, command.args[0]); }
  else if (name == "gotoLocal") { goto(page, context, "http://localhost:" + context.localPort + command.args[0]); }
  else if (name == "assert") { assert(page, context, command.args[0], command.args[1], command.args[2], command.args[3]); }
  else if (name == "close") { close(page, context); }
  else if (name == "serve") { serve(context, command.args[0], context.localPort); }
  else if (name == "stub") { stub(context, command.args[0], command.args[1], context.localPort); }
  else if (name == "init") { init(context); }
  else { respond(page, context, ["don't know how to process request: " + JSON.stringify(request) ]); }
});

//TODO: add start time, to capture duration ...
//TODO: rename to notifyElm or something ...
//TODO: make screenshot be a command and issue it from elm
//TODO: give it an id and attempt to highlight it in the screenshot
function respond(page, context, failures) {
  var y = Date.now()
  var x = y.toString()
//  console.log(x)
  var response = { context:context, failures:failures, updated:x }
  //TODO: we could continue to serve locally on context.localPort, it might be interesting for debugging test failures ...
  //TODO: just need a stayOpenOnFailure
  var screenshot = page != null && (screenshotAllSteps || (screenshotFailures && failures.length > 0) )
  if (screenshot) page.render(started + '/' + context.scriptId + '/' + context.stepId + '.png')
//  if (screenshot) console.log(page.plainText)
  app.ports.responses.send(response);
}

function init(context) {
  context.localPort = nextPort; nextPort = nextPort + 1;
  respond(null, context, []);
}

function goto(page, context, url) {
  page.open(url, function(status) {
    if (status !== 'success') { respond(page, context, [status + ' for ' + url]) }
    else { respond(page, context, []) }
  });
}

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
  return page.evaluate(function(theSelector) { //TIP: http://stackoverflow.com/questions/19669786/check-if-element-is-visible-in-dom
    var e = document.querySelectorAll(theSelector)
    return e.length == 1 && !!( e[0].offsetWidth || e[0].offsetHeight || e[0].getClientRects().length ); // aka visible
  }, selector);
}

function describeFailure(page, selector) {
  return page.evaluate(function(theSelector) {
    return "expected 1 element for " + theSelector + " found " + document.querySelectorAll(theSelector).length;
  }, selector);
}

function close(page, context) {
  respond(page, context, []);
  page.close();
  //TODO: pull out a separate exit
  console.log("Done " + (new Date().getTime() - started) + "ms.");
  phantom.exit();
}

function stub(context, path, content, port) {
  stubs[(port + ":" + path)] = content;
  respond(null, context, []);
}

//TODO: content-type c/should probably be passed in for stubs (or based on extension)
//TODO: should better handle fqn?queryString
function serve(context, path, port) {
  var service = server.create().listen(port, { keepAlive: true }, function (request, response) {
    var fqn = path + request.url;
    var key = port + ":" + request.url;

    if (stubs[key] !== undefined) { r = {body: stubs[key], code: 200}; }
    else if (fs.exists(fqn)) { r = {body: fs.read(fqn), code: 200}; }
    else { r = {body: "", code: 404}; }

    response.statusCode = r.code;
    response.headers = { 'Cache': 'no-cache', 'Content-Length': r.body.length, 'Content-Type': 'text/html' };
    response.write(r.body);
    response.close();
  });

  if (!service) { console.log('Error: Could not create web server listening on port ' + port); phantom.exit(); }
  respond(null, context, [])
}