(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.bundle = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
/*!
  * domready (c) Dustin Diaz 2014 - License MIT
  */
!function (name, definition) {

  if (typeof module != 'undefined') module.exports = definition()
  else if (typeof define == 'function' && typeof define.amd == 'object') define(definition)
  else this[name] = definition()

}('domready', function () {

  var fns = [], listener
    , doc = document
    , hack = doc.documentElement.doScroll
    , domContentLoaded = 'DOMContentLoaded'
    , loaded = (hack ? /^loaded|^c/ : /^loaded|^i|^c/).test(doc.readyState)


  if (!loaded)
  doc.addEventListener(domContentLoaded, listener = function () {
    doc.removeEventListener(domContentLoaded, listener)
    loaded = 1
    while (listener = fns.shift()) listener()
  })

  return function (fn) {
    loaded ? setTimeout(fn, 0) : fns.push(fn)
  }

});

},{}],2:[function(require,module,exports){
'use strict'

var toPX = require('to-px')

module.exports = mouseWheelListen

function mouseWheelListen(element, callback, noScroll) {
  if(typeof element === 'function') {
    noScroll = !!callback
    callback = element
    element = window
  }
  var lineHeight = toPX('ex', element)
  var listener = function(ev) {
    if(noScroll) {
      ev.preventDefault()
    }
    var dx = ev.deltaX || 0
    var dy = ev.deltaY || 0
    var dz = ev.deltaZ || 0
    var mode = ev.deltaMode
    var scale = 1
    switch(mode) {
      case 1:
        scale = lineHeight
      break
      case 2:
        scale = window.innerHeight
      break
    }
    dx *= scale
    dy *= scale
    dz *= scale
    if(dx || dy || dz) {
      return callback(dx, dy, dz, ev)
    }
  }
  element.addEventListener('wheel', listener)
  return listener
}

},{"to-px":5}],3:[function(require,module,exports){
module.exports = function parseUnit(str, out) {
    if (!out)
        out = [ 0, '' ]

    str = String(str)
    var num = parseFloat(str, 10)
    out[0] = num
    out[1] = str.match(/[\d.\-\+]*\s*(.*)/)[1] || ''
    return out
}
},{}],4:[function(require,module,exports){
!function(t,e){"object"==typeof exports&&"object"==typeof module?module.exports=e():"function"==typeof define&&define.amd?define([],e):"object"==typeof exports?exports.rollbar=e():t.rollbar=e()}(this,function(){return function(t){function e(n){if(r[n])return r[n].exports;var o=r[n]={exports:{},id:n,loaded:!1};return t[n].call(o.exports,o,o.exports,e),o.loaded=!0,o.exports}var r={};return e.m=t,e.c=r,e.p="",e(0)}([function(t,e,r){t.exports=r(1)},function(t,e,r){"use strict";var n=r(2),o="undefined"!=typeof window&&window._rollbarConfig,i=o&&o.globalAlias||"Rollbar",a="undefined"!=typeof window&&window[i]&&"function"==typeof window[i].shimId&&void 0!==window[i].shimId();if("undefined"==typeof window||window._rollbarStartTime||(window._rollbarStartTime=(new Date).getTime()),!a&&o){var s=new n(o);window[i]=s}else"undefined"!=typeof window?(window.rollbar=n,window._rollbarDidLoad=!0):"undefined"!=typeof self&&(self.rollbar=n,self._rollbarDidLoad=!0);t.exports=n},function(t,e,r){"use strict";function n(t,e){this.options=c.handleOptions(x,t);var r=new l(this.options,h,d);this.client=e||new u(this.options,r,p,"browser");var n="undefined"!=typeof window&&window||"undefined"!=typeof self&&self,o="undefined"!=typeof document&&document;i(this.client.notifier),a(this.client.queue),(this.options.captureUncaught||this.options.handleUncaughtExceptions)&&(f.captureUncaughtExceptions(n,this),f.wrapGlobals(n,this)),(this.options.captureUnhandledRejections||this.options.handleUnhandledRejections)&&f.captureUnhandledRejections(n,this),this.instrumenter=new w(this.options,this.client.telemeter,this,n,o),this.instrumenter.instrument()}function o(t){var e="Rollbar is not initialized";p.error(e),t&&t(new Error(e))}function i(t){t.addTransform(m.handleItemWithError).addTransform(m.ensureItemHasSomethingToSay).addTransform(m.addBaseInfo).addTransform(m.addRequestInfo(window)).addTransform(m.addClientInfo(window)).addTransform(m.addPluginInfo(window)).addTransform(m.addBody).addTransform(g.addMessageWithError).addTransform(g.addTelemetryData).addTransform(g.addConfigToPayload).addTransform(m.scrubPayload).addTransform(g.userTransform(p)).addTransform(g.itemToPayload)}function a(t){t.addPredicate(y.checkLevel).addPredicate(v.checkIgnore).addPredicate(y.userCheckIgnore(p)).addPredicate(y.urlIsNotBlacklisted(p)).addPredicate(y.urlIsWhitelisted(p)).addPredicate(y.messageIsIgnored(p))}function s(t){for(var e=0,r=t.length;e<r;++e)if(c.isFunction(t[e]))return t[e]}var u=r(3),c=r(5),l=r(11),p=r(13),f=r(16),h=r(17),d=r(19),m=r(20),g=r(24),v=r(25),y=r(26),b=r(21),w=r(27),_=null;n.init=function(t,e){return _?_.global(t).configure(t):_=new n(t,e)},n.prototype.global=function(t){return this.client.global(t),this},n.global=function(t){return _?_.global(t):void o()},n.prototype.configure=function(t,e){var r=this.options,n={};return e&&(n={payload:e}),this.options=c.handleOptions(r,t,n),this.client.configure(this.options,e),this.instrumenter.configure(this.options),this},n.configure=function(t,e){return _?_.configure(t,e):void o()},n.prototype.lastError=function(){return this.client.lastError},n.lastError=function(){return _?_.lastError():void o()},n.prototype.log=function(){var t=this._createItem(arguments),e=t.uuid;return this.client.log(t),{uuid:e}},n.log=function(){if(_)return _.log.apply(_,arguments);var t=s(arguments);o(t)},n.prototype.debug=function(){var t=this._createItem(arguments),e=t.uuid;return this.client.debug(t),{uuid:e}},n.debug=function(){if(_)return _.debug.apply(_,arguments);var t=s(arguments);o(t)},n.prototype.info=function(){var t=this._createItem(arguments),e=t.uuid;return this.client.info(t),{uuid:e}},n.info=function(){if(_)return _.info.apply(_,arguments);var t=s(arguments);o(t)},n.prototype.warn=function(){var t=this._createItem(arguments),e=t.uuid;return this.client.warn(t),{uuid:e}},n.warn=function(){if(_)return _.warn.apply(_,arguments);var t=s(arguments);o(t)},n.prototype.warning=function(){var t=this._createItem(arguments),e=t.uuid;return this.client.warning(t),{uuid:e}},n.warning=function(){if(_)return _.warning.apply(_,arguments);var t=s(arguments);o(t)},n.prototype.error=function(){var t=this._createItem(arguments),e=t.uuid;return this.client.error(t),{uuid:e}},n.error=function(){if(_)return _.error.apply(_,arguments);var t=s(arguments);o(t)},n.prototype.critical=function(){var t=this._createItem(arguments),e=t.uuid;return this.client.critical(t),{uuid:e}},n.critical=function(){if(_)return _.critical.apply(_,arguments);var t=s(arguments);o(t)},n.prototype.handleUncaughtException=function(t,e,r,n,o,i){var a,s=c.makeUnhandledStackInfo(t,e,r,n,o,"onerror","uncaught exception",b);c.isError(o)?(a=this._createItem([t,o,i]),a._unhandledStackInfo=s):c.isError(e)?(a=this._createItem([t,e,i]),a._unhandledStackInfo=s):(a=this._createItem([t,i]),a.stackInfo=s),a.level=this.options.uncaughtErrorLevel,a._isUncaught=!0,this.client.log(a)},n.prototype.handleUnhandledRejection=function(t,e){var r="unhandled rejection was null or undefined!";if(t)if(t.message)r=t.message;else{var n=c.stringify(t);n.value&&(r=n.value)}var o,i=t&&t._rollbarContext||e&&e._rollbarContext;c.isError(t)?o=this._createItem([r,t,i]):(o=this._createItem([r,t,i]),o.stackInfo=c.makeUnhandledStackInfo(r,"",0,0,null,"unhandledrejection","",b)),o.level=this.options.uncaughtErrorLevel,o._isUncaught=!0,o._originalArgs=o._originalArgs||[],o._originalArgs.push(e),this.client.log(o)},n.prototype.wrap=function(t,e,r){try{var n;if(n=c.isFunction(e)?e:function(){return e||{}},!c.isFunction(t))return t;if(t._isWrap)return t;if(!t._rollbar_wrapped&&(t._rollbar_wrapped=function(){r&&c.isFunction(r)&&r.apply(this,arguments);try{return t.apply(this,arguments)}catch(r){var e=r;throw e&&(c.isType(e,"string")&&(e=new String(e)),e._rollbarContext=n()||{},e._rollbarContext._wrappedSource=t.toString(),window._rollbarWrappedError=e),e}},t._rollbar_wrapped._isWrap=!0,t.hasOwnProperty))for(var o in t)t.hasOwnProperty(o)&&(t._rollbar_wrapped[o]=t[o]);return t._rollbar_wrapped}catch(e){return t}},n.wrap=function(t,e){return _?_.wrap(t,e):void o()},n.prototype.captureEvent=function(t,e){return this.client.captureEvent(t,e)},n.captureEvent=function(t,e){return _?_.captureEvent(t,e):void o()},n.prototype.captureDomContentLoaded=function(t,e){return e||(e=new Date),this.client.captureDomContentLoaded(e)},n.prototype.captureLoad=function(t,e){return e||(e=new Date),this.client.captureLoad(e)},n.prototype._createItem=function(t){return c.createItem(t,p,this)};var x={version:"2.5.1",scrubFields:["pw","pass","passwd","password","secret","confirm_password","confirmPassword","password_confirmation","passwordConfirmation","access_token","accessToken","secret_key","secretKey","secretToken","cc-number","card number","cardnumber","cardnum","ccnum","ccnumber","cc num","creditcardnumber","credit card number","newcreditcardnumber","new credit card","creditcardno","credit card no","card#","card #","cc-csc","cvc2","cvv2","ccv2","security code","card verification","name on credit card","name on card","nameoncard","cardholder","card holder","name des karteninhabers","card type","cardtype","cc type","cctype","payment type","expiration date","expirationdate","expdate","cc-exp"],logLevel:"debug",reportLevel:"debug",uncaughtErrorLevel:"error",endpoint:"api.rollbar.com/api/1/item/",verbose:!1,enabled:!0,sendConfig:!1,includeItemsInTelemetry:!0,captureIp:!0};t.exports=n},function(t,e,r){"use strict";function n(t,e,r,o){this.options=c.merge(t),this.logger=r,n.rateLimiter.configureGlobal(this.options),n.rateLimiter.setPlatformOptions(o,this.options),this.queue=new a(n.rateLimiter,e,r,this.options),this.notifier=new s(this.queue,this.options),this.telemeter=new u(this.options),this.lastError=null,this.lastErrorHash="none"}function o(t){var e=t.message||"",r=(t.err||{}).stack||String(t.err);return e+"::"+r}var i=r(4),a=r(8),s=r(9),u=r(10),c=r(5),l={maxItems:0,itemsPerMinute:60};n.rateLimiter=new i(l),n.prototype.global=function(t){return n.rateLimiter.configureGlobal(t),this},n.prototype.configure=function(t,e){var r=this.options,n={};return e&&(n={payload:e}),this.options=c.merge(r,t,n),this.notifier&&this.notifier.configure(this.options),this.telemeter&&this.telemeter.configure(this.options),this.global(this.options),this},n.prototype.log=function(t){var e=this._defaultLogLevel();return this._log(e,t)},n.prototype.debug=function(t){this._log("debug",t)},n.prototype.info=function(t){this._log("info",t)},n.prototype.warn=function(t){this._log("warning",t)},n.prototype.warning=function(t){this._log("warning",t)},n.prototype.error=function(t){this._log("error",t)},n.prototype.critical=function(t){this._log("critical",t)},n.prototype.wait=function(t){this.queue.wait(t)},n.prototype.captureEvent=function(t,e){return this.telemeter.captureEvent(t,e)},n.prototype.captureDomContentLoaded=function(t){return this.telemeter.captureDomContentLoaded(t)},n.prototype.captureLoad=function(t){return this.telemeter.captureLoad(t)},n.prototype._log=function(t,e){var r;if(e.callback&&(r=e.callback,delete e.callback),this._sameAsLastError(e)){if(r){var n=new Error("ignored identical item");n.item=e,r(n)}}else try{e.level=e.level||t,this.telemeter._captureRollbarItem(e),e.telemetryEvents=this.telemeter.copyEvents(),this.notifier.log(e,r)}catch(t){this.logger.error(t)}},n.prototype._defaultLogLevel=function(){return this.options.logLevel||"debug"},n.prototype._sameAsLastError=function(t){if(!t._isUncaught)return!1;var e=o(t);return this.lastErrorHash===e||(this.lastError=t.err,this.lastErrorHash=e,!1)},t.exports=n},function(t,e,r){"use strict";function n(t){this.startTime=s.now(),this.counter=0,this.perMinCounter=0,this.platform=null,this.platformOptions={},this.configureGlobal(t)}function o(t,e,r){return!t.ignoreRateLimit&&e>=1&&r>e}function i(t,e,r,n,o,i,s){var u=null;return r&&(r=new Error(r)),r||n||(u=a(t,e,o,i,s)),{error:r,shouldSend:n,payload:u}}function a(t,e,r,n,o){var i,a=e.environment||e.payload&&e.payload.environment;i=o?"item per minute limit reached, ignoring errors until timeout":"maxItems has been hit, ignoring errors until reset.";var s={body:{message:{body:i,extra:{maxItems:r,itemsPerMinute:n}}},language:"javascript",environment:a,notifier:{version:e.notifier&&e.notifier.version||e.version}};return"browser"===t?(s.platform="browser",s.framework="browser-js",s.notifier.name="rollbar-browser-js"):"server"===t?(s.framework=e.framework||"node-js",s.notifier.name=e.notifier.name):"react-native"===t&&(s.framework=e.framework||"react-native",s.notifier.name=e.notifier.name),s}var s=r(5);n.globalSettings={startTime:s.now(),maxItems:void 0,itemsPerMinute:void 0},n.prototype.configureGlobal=function(t){void 0!==t.startTime&&(n.globalSettings.startTime=t.startTime),void 0!==t.maxItems&&(n.globalSettings.maxItems=t.maxItems),void 0!==t.itemsPerMinute&&(n.globalSettings.itemsPerMinute=t.itemsPerMinute)},n.prototype.shouldSend=function(t,e){e=e||s.now();var r=e-this.startTime;(r<0||r>=6e4)&&(this.startTime=e,this.perMinCounter=0);var a=n.globalSettings.maxItems,u=n.globalSettings.itemsPerMinute;if(o(t,a,this.counter))return i(this.platform,this.platformOptions,a+" max items reached",!1);if(o(t,u,this.perMinCounter))return i(this.platform,this.platformOptions,u+" items per minute reached",!1);this.counter++,this.perMinCounter++;var c=!o(t,a,this.counter),l=c;return c=c&&!o(t,u,this.perMinCounter),i(this.platform,this.platformOptions,null,c,a,u,l)},n.prototype.setPlatformOptions=function(t,e){this.platform=t,this.platformOptions=e},t.exports=n},function(t,e,r){"use strict";function n(){if(!F&&(F=!0,c(JSON)&&(s(JSON.stringify)&&(A.stringify=JSON.stringify),s(JSON.parse)&&(A.parse=JSON.parse)),!a(A.stringify)||!a(A.parse))){var t=r(7);t(A)}}function o(t,e){return e===i(t)}function i(t){var e=typeof t;return"object"!==e?e:t?t instanceof Error?"error":{}.toString.call(t).match(/\s([a-zA-Z]+)/)[1].toLowerCase():"null"}function a(t){return o(t,"function")}function s(t){var e=/[\\^$.*+?()[\]{}|]/g,r=Function.prototype.toString.call(Object.prototype.hasOwnProperty).replace(e,"\\$&").replace(/hasOwnProperty|(function).*?(?=\\\()| for .+?(?=\\\])/g,"$1.*?"),n=RegExp("^"+r+"$");return u(t)&&n.test(t)}function u(t){var e=typeof t;return null!=t&&("object"==e||"function"==e)}function c(t){return!o(t,"undefined")}function l(t){var e=i(t);return"object"===e||"array"===e}function p(t){return o(t,"error")}function f(t,e,r){var n,i,a,s=o(t,"object"),u=o(t,"array"),c=[];if(s&&r.indexOf(t)!==-1)return t;if(r.push(t),s)for(n in t)Object.prototype.hasOwnProperty.call(t,n)&&c.push(n);else if(u)for(a=0;a<t.length;++a)c.push(a);var l=s?{}:[],p=!0;for(a=0;a<c.length;++a)n=c[a],i=t[n],l[n]=e(n,i,r),p=p&&l[n]===t[n];return 0==c.length||p?t:l}function h(){return"********"}function d(){var t=N(),e="xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g,function(e){var r=(t+16*Math.random())%16|0;return t=Math.floor(t/16),("x"===e?r:7&r|8).toString(16)});return e}function m(t){var e=g(t);return e?(""===e.anchor&&(e.source=e.source.replace("#","")),t=e.source.replace("?"+e.query,"")):"(unknown)"}function g(t){if(o(t,"string")){for(var e=P,r=e.parser[e.strictMode?"strict":"loose"].exec(t),n={},i=e.key.length;i--;)n[e.key[i]]=r[i]||"";return n[e.q.name]={},n[e.key[12]].replace(e.q.parser,function(t,r,o){r&&(n[e.q.name][r]=o)}),n}}function v(t,e,r){r=r||{},r.access_token=t;var n,o=[];for(n in r)Object.prototype.hasOwnProperty.call(r,n)&&o.push([n,r[n]].join("="));var i="?"+o.sort().join("&");e=e||{},e.path=e.path||"";var a,s=e.path.indexOf("?"),u=e.path.indexOf("#");s!==-1&&(u===-1||u>s)?(a=e.path,e.path=a.substring(0,s)+i+"&"+a.substring(s+1)):u!==-1?(a=e.path,e.path=a.substring(0,u)+i+a.substring(u)):e.path=e.path+i}function y(t,e){if(e=e||t.protocol,!e&&t.port&&(80===t.port?e="http:":443===t.port&&(e="https:")),e=e||"https:",!t.hostname)return null;var r=e+"//"+t.hostname;return t.port&&(r=r+":"+t.port),t.path&&(r+=t.path),r}function b(t,e){var r,n;try{r=A.stringify(t)}catch(o){if(e&&a(e))try{r=e(t)}catch(t){n=t}else n=o}return{error:n,value:r}}function w(t){var e,r;try{e=A.parse(t)}catch(t){r=t}return{error:r,value:e}}function _(t,e,r,n,o,i,a,s){var u={url:e||"",line:r,column:n};u.func=s.guessFunctionName(u.url,u.line),u.context=s.gatherContext(u.url,u.line);var c=document&&document.location&&document.location.href,l=window&&window.navigator&&window.navigator.userAgent;return{mode:i,message:o?String(o):t||a,url:c,stack:[u],useragent:l}}function x(t,e){return function(r,n){try{e(r,n)}catch(e){t.error(e)}}}function k(t,e,r,n,o){for(var a,s,u,c,l,p,f=[],h=0,m=t.length;h<m;++h){p=t[h];var g=i(p);switch(g){case"undefined":break;case"string":a?f.push(p):a=p;break;case"function":c=x(e,p);break;case"date":f.push(p);break;case"error":case"domexception":s?f.push(p):s=p;break;case"object":case"array":if(p instanceof Error||"undefined"!=typeof DOMException&&p instanceof DOMException){s?f.push(p):s=p;break}if(n&&"object"===g&&!l){for(var v=0,y=n.length;v<y;++v)if(void 0!==p[n[v]]){l=p;break}if(l)break}u?f.push(p):u=p;break;default:if(p instanceof Error||"undefined"!=typeof DOMException&&p instanceof DOMException){s?f.push(p):s=p;break}f.push(p)}}f.length>0&&(u=R(u),u.extraArgs=f);var b={message:a,err:s,custom:u,timestamp:N(),callback:c,uuid:d()};return u&&void 0!==u.level&&(b.level=u.level,delete u.level),n&&l&&(b.request=l),o&&(b.lambdaContext=o),b._originalArgs=t,b}function E(t,e){if(t){var r=e.split("."),n=t;try{for(var o=0,i=r.length;o<i;++o)n=n[r[o]]}catch(t){n=void 0}return n}}function I(t,e,r){if(t){var n=e.split("."),o=n.length;if(!(o<1)){if(1===o)return void(t[n[0]]=r);try{for(var i=t[n[0]]||{},a=i,s=1;s<o-1;s++)i[n[s]]=i[n[s]]||{},i=i[n[s]];i[n[o-1]]=r,t[n[0]]=a}catch(t){return}}}}function T(t,e){function r(t,e){return e+h()}function n(t){var e;if(o(t,"string"))for(e=0;e<u.length;++e)t=t.replace(u[e],r);return t}function i(t,e){var r;for(r=0;r<s.length;++r)if(s[r].test(t)){e=h();break}return e}function a(t,e,r){var s=i(t,e);return s===e?o(e,"object")||o(e,"array")?f(e,a,r):n(s):s}e=e||[];var s=S(e),u=O(e);return f(t,a,[])}function S(t){for(var e,r=[],n=0;n<t.length;++n)e="^\\[?(%5[bB])?"+t[n]+"\\[?(%5[bB])?\\]?(%5[dD])?$",r.push(new RegExp(e,"i"));return r}function O(t){for(var e,r=[],n=0;n<t.length;++n)e="\\[?(%5[bB])?"+t[n]+"\\[?(%5[bB])?\\]?(%5[dD])?",r.push(new RegExp("("+e+"=)([^&\\n]+)","igm"));return r}function L(t){var e,r,n,o=[];for(e=0,r=t.length;e<r;e++){switch(n=t[e],i(n)){case"object":n=b(n),n=n.error||n.value,n.length>500&&(n=n.substr(0,497)+"...");break;case"null":n="null";break;case"undefined":n="undefined";break;case"symbol":n=n.toString()}o.push(n)}return o.join(" ")}function N(){return Date.now?+Date.now():+new Date}function C(t,e){if(t&&t.user_ip&&e!==!0){var r=t.user_ip;if(e)try{var n;if(r.indexOf(".")!==-1)n=r.split("."),n.pop(),n.push("0"),r=n.join(".");else if(r.indexOf(":")!==-1){if(n=r.split(":"),n.length>2){var o=n.slice(0,3),i=o[2].indexOf("/");i!==-1&&(o[2]=o[2].substring(0,i));var a="0000:0000:0000:0000:0000";r=o.concat(a).join(":")}}else r=null}catch(t){r=null}else r=null;t.user_ip=r}}function j(t,e,r){var n=R(t,e,r);return!e||e.overwriteScrubFields?n:(e.scrubFields&&(n.scrubFields=(t.scrubFields||[]).concat(e.scrubFields)),n)}var R=r(6),A={},F=!1;n();var q={debug:0,info:1,warning:2,error:3,critical:4},P={strictMode:!1,key:["source","protocol","authority","userInfo","user","password","host","port","relative","path","directory","file","query","anchor"],q:{name:"queryKey",parser:/(?:^|&)([^&=]*)=?([^&]*)/g},parser:{strict:/^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,loose:/^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/}};t.exports={addParamsAndAccessTokenToPath:v,createItem:k,filterIp:C,formatArgsAsString:L,formatUrl:y,get:E,handleOptions:j,isError:p,isFunction:a,isIterable:l,isNativeFunction:s,isType:o,jsonParse:w,LEVELS:q,makeUnhandledStackInfo:_,merge:R,now:N,redact:h,sanitizeUrl:m,scrub:T,set:I,stringify:b,traverse:f,typeName:i,uuid4:d}},function(t,e){"use strict";function r(){var t,e,n,o,a,s={},u=null,c=arguments.length;for(t=0;t<c;t++)if(u=arguments[t],null!=u)for(a in u)e=s[a],n=u[a],s!==n&&(n&&i(n)?(o=e&&i(e)?e:{},s[a]=r(o,n)):"undefined"!=typeof n&&(s[a]=n));return s}var n=Object.prototype.hasOwnProperty,o=Object.prototype.toString,i=function(t){if(!t||"[object Object]"!==o.call(t))return!1;var e=n.call(t,"constructor"),r=t.constructor&&t.constructor.prototype&&n.call(t.constructor.prototype,"isPrototypeOf");if(t.constructor&&!e&&!r)return!1;var i;for(i in t);return"undefined"==typeof i||n.call(t,i)};t.exports=r},function(t,e){var r=function(t){function e(t){return t<10?"0"+t:t}function r(){return this.valueOf()}function n(t){return i.lastIndex=0,i.test(t)?'"'+t.replace(i,function(t){var e=u[t];return"string"==typeof e?e:"\\u"+("0000"+t.charCodeAt(0).toString(16)).slice(-4)})+'"':'"'+t+'"'}function o(t,e){var r,i,u,l,p,f=a,h=e[t];switch(h&&"object"==typeof h&&"function"==typeof h.toJSON&&(h=h.toJSON(t)),"function"==typeof c&&(h=c.call(e,t,h)),typeof h){case"string":return n(h);case"number":return isFinite(h)?String(h):"null";case"boolean":case"null":return String(h);case"object":if(!h)return"null";if(a+=s,p=[],"[object Array]"===Object.prototype.toString.apply(h)){for(l=h.length,r=0;r<l;r+=1)p[r]=o(r,h)||"null";return u=0===p.length?"[]":a?"[\n"+a+p.join(",\n"+a)+"\n"+f+"]":"["+p.join(",")+"]",a=f,u}if(c&&"object"==typeof c)for(l=c.length,r=0;r<l;r+=1)"string"==typeof c[r]&&(i=c[r],u=o(i,h),u&&p.push(n(i)+(a?": ":":")+u));else for(i in h)Object.prototype.hasOwnProperty.call(h,i)&&(u=o(i,h),u&&p.push(n(i)+(a?": ":":")+u));return u=0===p.length?"{}":a?"{\n"+a+p.join(",\n"+a)+"\n"+f+"}":"{"+p.join(",")+"}",a=f,u}}var i=/[\\"\u0000-\u001f\u007f-\u009f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g;"function"!=typeof Date.prototype.toJSON&&(Date.prototype.toJSON=function(){return isFinite(this.valueOf())?this.getUTCFullYear()+"-"+e(this.getUTCMonth()+1)+"-"+e(this.getUTCDate())+"T"+e(this.getUTCHours())+":"+e(this.getUTCMinutes())+":"+e(this.getUTCSeconds())+"Z":null},Boolean.prototype.toJSON=r,Number.prototype.toJSON=r,String.prototype.toJSON=r);var a,s,u,c;"function"!=typeof t.stringify&&(u={"\b":"\\b","\t":"\\t","\n":"\\n","\f":"\\f","\r":"\\r",'"':'\\"',"\\":"\\\\"},t.stringify=function(t,e,r){var n;if(a="",s="","number"==typeof r)for(n=0;n<r;n+=1)s+=" ";else"string"==typeof r&&(s=r);if(c=e,e&&"function"!=typeof e&&("object"!=typeof e||"number"!=typeof e.length))throw new Error("JSON.stringify");return o("",{"":t})}),"function"!=typeof t.parse&&(t.parse=function(){function t(t){return t.replace(/\\(?:u(.{4})|([^u]))/g,function(t,e,r){return e?String.fromCharCode(parseInt(e,16)):a[r]})}var e,r,n,o,i,a={"\\":"\\",'"':'"',"/":"/",t:"\t",n:"\n",r:"\r",f:"\f",b:"\b"},s={go:function(){e="ok"},firstokey:function(){o=i,e="colon"},okey:function(){o=i,e="colon"},ovalue:function(){e="ocomma"},firstavalue:function(){e="acomma"},avalue:function(){e="acomma"}},u={go:function(){e="ok"},ovalue:function(){e="ocomma"},firstavalue:function(){e="acomma"},avalue:function(){e="acomma"}},c={"{":{go:function(){r.push({state:"ok"}),n={},e="firstokey"},ovalue:function(){r.push({container:n,state:"ocomma",key:o}),n={},e="firstokey"},firstavalue:function(){r.push({container:n,state:"acomma"}),n={},e="firstokey"},avalue:function(){r.push({container:n,state:"acomma"}),n={},e="firstokey"}},"}":{firstokey:function(){var t=r.pop();i=n,n=t.container,o=t.key,e=t.state},ocomma:function(){var t=r.pop();n[o]=i,i=n,n=t.container,o=t.key,e=t.state}},"[":{go:function(){r.push({state:"ok"}),n=[],e="firstavalue"},ovalue:function(){r.push({container:n,state:"ocomma",key:o}),n=[],e="firstavalue"},firstavalue:function(){r.push({container:n,state:"acomma"}),n=[],e="firstavalue"},avalue:function(){r.push({container:n,state:"acomma"}),n=[],e="firstavalue"}},"]":{firstavalue:function(){var t=r.pop();i=n,n=t.container,o=t.key,e=t.state},acomma:function(){var t=r.pop();n.push(i),i=n,n=t.container,o=t.key,e=t.state}},":":{colon:function(){if(Object.hasOwnProperty.call(n,o))throw new SyntaxError("Duplicate key '"+o+'"');e="ovalue"}},",":{ocomma:function(){n[o]=i,e="okey"},acomma:function(){n.push(i),e="avalue"}},true:{go:function(){i=!0,e="ok"},ovalue:function(){i=!0,e="ocomma"},firstavalue:function(){i=!0,e="acomma"},avalue:function(){i=!0,e="acomma"}},false:{go:function(){i=!1,e="ok"},ovalue:function(){i=!1,e="ocomma"},firstavalue:function(){i=!1,e="acomma"},avalue:function(){i=!1,e="acomma"}},null:{go:function(){i=null,e="ok"},ovalue:function(){i=null,e="ocomma"},firstavalue:function(){i=null,e="acomma"},avalue:function(){i=null,e="acomma"}}};return function(n,o){var a,l=/^[\u0020\t\n\r]*(?:([,:\[\]{}]|true|false|null)|(-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)|"((?:[^\r\n\t\\\"]|\\(?:["\\\/trnfb]|u[0-9a-fA-F]{4}))*)")/;e="go",r=[];try{for(;;){if(a=l.exec(n),!a)break;a[1]?c[a[1]][e]():a[2]?(i=+a[2],u[e]()):(i=t(a[3]),s[e]()),n=n.slice(a[0].length)}}catch(t){e=t}if("ok"!==e||/[^\u0020\t\n\r]/.test(n))throw e instanceof SyntaxError?e:new SyntaxError("JSON");return"function"==typeof o?function t(e,r){var n,a,s=e[r];if(s&&"object"==typeof s)for(n in i)Object.prototype.hasOwnProperty.call(s,n)&&(a=t(s,n),void 0!==a?s[n]=a:delete s[n]);return o.call(e,r,s)}({"":i},""):i}}())};t.exports=r},function(t,e,r){"use strict";function n(t,e,r,n){this.rateLimiter=t,this.api=e,this.logger=r,this.options=n,this.predicates=[],this.pendingItems=[],this.pendingRequests=[],this.retryQueue=[],this.retryHandle=null,this.waitCallback=null,this.waitIntervalID=null}var o=r(5);n.prototype.configure=function(t){this.api&&this.api.configure(t);var e=this.options;return this.options=o.merge(e,t),this},n.prototype.addPredicate=function(t){return o.isFunction(t)&&this.predicates.push(t),this},n.prototype.addPendingItem=function(t){this.pendingItems.push(t)},n.prototype.removePendingItem=function(t){var e=this.pendingItems.indexOf(t);e!==-1&&this.pendingItems.splice(e,1)},n.prototype.addItem=function(t,e,r,n){e&&o.isFunction(e)||(e=function(){});var i=this._applyPredicates(t);if(i.stop)return this.removePendingItem(n),void e(i.err);this._maybeLog(t,r),this.removePendingItem(n),this.pendingRequests.push(t);try{this._makeApiRequest(t,function(r,n){this._dequeuePendingRequest(t),e(r,n)}.bind(this))}catch(r){this._dequeuePendingRequest(t),e(r)}},n.prototype.wait=function(t){o.isFunction(t)&&(this.waitCallback=t,this._maybeCallWait()||(this.waitIntervalID&&(this.waitIntervalID=clearInterval(this.waitIntervalID)),this.waitIntervalID=setInterval(function(){this._maybeCallWait()}.bind(this),500)))},n.prototype._applyPredicates=function(t){for(var e=null,r=0,n=this.predicates.length;r<n;r++)if(e=this.predicates[r](t,this.options),!e||void 0!==e.err)return{stop:!0,err:e.err};return{stop:!1,err:null}},n.prototype._makeApiRequest=function(t,e){var r=this.rateLimiter.shouldSend(t);r.shouldSend?this.api.postItem(t,function(r,n){r?this._maybeRetry(r,t,e):e(r,n)}.bind(this)):r.error?e(r.error):this.api.postItem(r.payload,e)};var i=["ECONNRESET","ENOTFOUND","ESOCKETTIMEDOUT","ETIMEDOUT","ECONNREFUSED","EHOSTUNREACH","EPIPE","EAI_AGAIN"];n.prototype._maybeRetry=function(t,e,r){var n=!1;if(this.options.retryInterval)for(var o=0,a=i.length;o<a;o++)if(t.code===i[o]){n=!0;break}n?this._retryApiRequest(e,r):r(t)},n.prototype._retryApiRequest=function(t,e){this.retryQueue.push({item:t,callback:e}),this.retryHandle||(this.retryHandle=setInterval(function(){for(;this.retryQueue.length;){var t=this.retryQueue.shift();this._makeApiRequest(t.item,t.callback)}}.bind(this),this.options.retryInterval))},n.prototype._dequeuePendingRequest=function(t){var e=this.pendingRequests.indexOf(t);e!==-1&&(this.pendingRequests.splice(e,1),this._maybeCallWait())},n.prototype._maybeLog=function(t,e){if(this.logger&&this.options.verbose){var r=e;if(r=r||o.get(t,"body.trace.exception.message"),r=r||o.get(t,"body.trace_chain.0.exception.message"))return void this.logger.error(r);r=o.get(t,"body.message.body"),r&&this.logger.log(r)}},n.prototype._maybeCallWait=function(){return!(!o.isFunction(this.waitCallback)||0!==this.pendingItems.length||0!==this.pendingRequests.length)&&(this.waitIntervalID&&(this.waitIntervalID=clearInterval(this.waitIntervalID)),this.waitCallback(),!0)},t.exports=n},function(t,e,r){"use strict";function n(t,e){this.queue=t,this.options=e,this.transforms=[]}var o=r(5);n.prototype.configure=function(t){this.queue&&this.queue.configure(t);var e=this.options;return this.options=o.merge(e,t),this},n.prototype.addTransform=function(t){return o.isFunction(t)&&this.transforms.push(t),this},n.prototype.log=function(t,e){if(e&&o.isFunction(e)||(e=function(){}),!this.options.enabled)return e(new Error("Rollbar is not enabled"));this.queue.addPendingItem(t);var r=t.err;this._applyTransforms(t,function(n,o){return n?(this.queue.removePendingItem(t),e(n,null)):void this.queue.addItem(o,e,r,t)}.bind(this))},n.prototype._applyTransforms=function(t,e){var r=-1,n=this.transforms.length,o=this.transforms,i=this.options,a=function(t,s){return t?void e(t,null):(r++,r===n?void e(null,s):void o[r](s,i,a))};a(null,t)},t.exports=n},function(t,e,r){"use strict";function n(t){this.queue=[],this.options=i.merge(t);var e=this.options.maxTelemetryEvents||a;this.maxQueueSize=Math.max(0,Math.min(e,a))}function o(t,e){if(e)return e;var r={error:"error",manual:"info"};return r[t]||"info"}var i=r(5),a=100;n.prototype.configure=function(t){var e=this.options;this.options=i.merge(e,t);var r=this.options.maxTelemetryEvents||a,n=Math.max(0,Math.min(r,a)),o=0;this.maxQueueSize>n&&(o=this.maxQueueSize-n),this.maxQueueSize=n,this.queue.splice(0,o)},n.prototype.copyEvents=function(){var t=Array.prototype.slice.call(this.queue,0);if(i.isFunction(this.options.filterTelemetry))try{for(var e=t.length;e--;)this.options.filterTelemetry(t[e])&&t.splice(e,1)}catch(t){this.options.filterTelemetry=null}return t},n.prototype.capture=function(t,e,r,n,a){var s={level:o(t,r),type:t,timestamp_ms:a||i.now(),body:e,source:"client"};n&&(s.uuid=n);try{if(i.isFunction(this.options.filterTelemetry)&&this.options.filterTelemetry(s))return!1}catch(t){this.options.filterTelemetry=null}return this.push(s),s},n.prototype.captureEvent=function(t,e,r){return this.capture("manual",t,e,r)},n.prototype.captureError=function(t,e,r,n){var o={message:t.message||String(t)};return t.stack&&(o.stack=t.stack),this.capture("error",o,e,r,n)},n.prototype.captureLog=function(t,e,r,n){return this.capture("log",{message:t},e,r,n)},n.prototype.captureNetwork=function(t,e,r,n){e=e||"xhr",t.subtype=t.subtype||e,n&&(t.request=n);var o=this.levelFromStatus(t.status_code);return this.capture("network",t,o,r)},n.prototype.levelFromStatus=function(t){return t>=200&&t<400?"info":0===t||t>=400?"error":"info"},n.prototype.captureDom=function(t,e,r,n,o){var i={subtype:t,element:e};return void 0!==r&&(i.value=r),void 0!==n&&(i.checked=n),this.capture("dom",i,"info",o)},n.prototype.captureNavigation=function(t,e,r){return this.capture("navigation",{from:t,to:e},"info",r)},n.prototype.captureDomContentLoaded=function(t){return this.capture("navigation",{subtype:"DOMContentLoaded"},"info",void 0,t&&t.getTime())},n.prototype.captureLoad=function(t){return this.capture("navigation",{subtype:"load"},"info",void 0,t&&t.getTime())},n.prototype.captureConnectivityChange=function(t,e){return this.captureNetwork({change:t},"connectivity",e)},n.prototype._captureRollbarItem=function(t){if(this.options.includeItemsInTelemetry)return t.err?this.captureError(t.err,t.level,t.uuid,t.timestamp):t.message?this.captureLog(t.message,t.level,t.uuid,t.timestamp):t.custom?this.capture("log",t.custom,t.level,t.uuid,t.timestamp):void 0},n.prototype.push=function(t){this.queue.push(t),this.queue.length>this.maxQueueSize&&this.queue.shift()},t.exports=n},function(t,e,r){"use strict";function n(t,e,r,n){this.options=t,this.transport=e,this.url=r,this.jsonBackup=n,this.accessToken=t.accessToken,this.transportOptions=o(t,r)}function o(t,e){return a.getTransportFromOptions(t,s,e)}var i=r(5),a=r(12),s={hostname:"api.rollbar.com",path:"/api/1/item/",search:null,version:"1",protocol:"https:",port:443};n.prototype.postItem=function(t,e){var r=a.transportOptions(this.transportOptions,"POST"),n=a.buildPayload(this.accessToken,t,this.jsonBackup);this.transport.post(this.accessToken,r,n,e)},n.prototype.configure=function(t){var e=this.oldOptions;return this.options=i.merge(e,t),this.transportOptions=o(this.options,this.url),void 0!==this.options.accessToken&&(this.accessToken=this.options.accessToken),this},t.exports=n},function(t,e,r){"use strict";function n(t,e,r){if(!s.isType(e.context,"string")){var n=s.stringify(e.context,r);n.error?e.context="Error: could not serialize 'context'":e.context=n.value||"",e.context.length>255&&(e.context=e.context.substr(0,255))}return{access_token:t,data:e}}function o(t,e,r){var n=e.hostname,o=e.protocol,i=e.port,a=e.path,s=e.search,u=t.proxy;if(t.endpoint){var c=r.parse(t.endpoint);n=c.hostname,o=c.protocol,i=c.port,a=c.pathname,s=c.search}return{hostname:n,protocol:o,port:i,path:a,search:s,proxy:u}}function i(t,e){var r=t.protocol||"https:",n=t.port||("http:"===r?80:"https:"===r?443:void 0),o=t.hostname,i=t.path;return t.search&&(i+=t.search),t.proxy&&(i=r+"//"+o+i,o=t.proxy.host||t.proxy.hostname,n=t.proxy.port,r=t.proxy.protocol||r),{protocol:r,hostname:o,path:i,port:n,method:e}}function a(t,e){var r=/\/$/.test(t),n=/^\//.test(e);return r&&n?e=e.substring(1):r||n||(e="/"+e),t+e}var s=r(5);t.exports={buildPayload:n,getTransportFromOptions:o,transportOptions:i,appendPathToPath:a}},function(t,e,r){"use strict";function n(){var t=Array.prototype.slice.call(arguments,0);t.unshift("Rollbar:"),a.ieVersion()<=8?console.error(s.formatArgsAsString(t)):console.error.apply(console,t);
}function o(){var t=Array.prototype.slice.call(arguments,0);t.unshift("Rollbar:"),a.ieVersion()<=8?console.info(s.formatArgsAsString(t)):console.info.apply(console,t)}function i(){var t=Array.prototype.slice.call(arguments,0);t.unshift("Rollbar:"),a.ieVersion()<=8?console.log(s.formatArgsAsString(t)):console.log.apply(console,t)}r(14);var a=r(15),s=r(5);t.exports={error:n,info:o,log:i}},function(t,e){!function(t){"use strict";t.console||(t.console={});for(var e,r,n=t.console,o=function(){},i=["memory"],a="assert,clear,count,debug,dir,dirxml,error,exception,group,groupCollapsed,groupEnd,info,log,markTimeline,profile,profiles,profileEnd,show,table,time,timeEnd,timeline,timelineEnd,timeStamp,trace,warn".split(",");e=i.pop();)n[e]||(n[e]={});for(;r=a.pop();)n[r]||(n[r]=o)}("undefined"==typeof window?this:window)},function(t,e){"use strict";function r(){var t;if(!document)return t;for(var e=3,r=document.createElement("div"),n=r.getElementsByTagName("i");r.innerHTML="<!--[if gt IE "+ ++e+"]><i></i><![endif]-->",n[0];);return e>4?e:t}var n={ieVersion:r};t.exports=n},function(t,e){"use strict";function r(t,e,r){if(t){var o;if("function"==typeof e._rollbarOldOnError)o=e._rollbarOldOnError;else if(t.onerror){for(o=t.onerror;o._rollbarOldOnError;)o=o._rollbarOldOnError;e._rollbarOldOnError=o}var i=function(){var r=Array.prototype.slice.call(arguments,0);n(t,e,o,r)};r&&(i._rollbarOldOnError=o),t.onerror=i}}function n(t,e,r,n){t._rollbarWrappedError&&(n[4]||(n[4]=t._rollbarWrappedError),n[5]||(n[5]=t._rollbarWrappedError._rollbarContext),t._rollbarWrappedError=null),e.handleUncaughtException.apply(e,n),r&&r.apply(t,n)}function o(t,e,r){if(t){"function"==typeof t._rollbarURH&&t._rollbarURH.belongsToShim&&t.removeEventListener("unhandledrejection",t._rollbarURH);var n=function(t){var r,n,o;try{r=t.reason}catch(t){r=void 0}try{n=t.promise}catch(t){n="[unhandledrejection] error getting `promise` from event"}try{o=t.detail,!r&&o&&(r=o.reason,n=o.promise)}catch(t){}r||(r="[unhandledrejection] error getting `reason` from event"),e&&e.handleUnhandledRejection&&e.handleUnhandledRejection(r,n)};n.belongsToShim=r,t._rollbarURH=n,t.addEventListener("unhandledrejection",n)}}function i(t,e,r){if(t){var n,o,i="EventTarget,Window,Node,ApplicationCache,AudioTrackList,ChannelMergerNode,CryptoOperation,EventSource,FileReader,HTMLUnknownElement,IDBDatabase,IDBRequest,IDBTransaction,KeyOperation,MediaController,MessagePort,ModalWindow,Notification,SVGElementInstance,Screen,TextTrack,TextTrackCue,TextTrackList,WebSocket,WebSocketWorker,Worker,XMLHttpRequest,XMLHttpRequestEventTarget,XMLHttpRequestUpload".split(",");for(n=0;n<i.length;++n)o=i[n],t[o]&&t[o].prototype&&a(e,t[o].prototype,r)}}function a(t,e,r){if(e.hasOwnProperty&&e.hasOwnProperty("addEventListener")){for(var n=e.addEventListener;n._rollbarOldAdd&&n.belongsToShim;)n=n._rollbarOldAdd;var o=function(e,r,o){n.call(this,e,t.wrap(r),o)};o._rollbarOldAdd=n,o.belongsToShim=r,e.addEventListener=o;for(var i=e.removeEventListener;i._rollbarOldRemove&&i.belongsToShim;)i=i._rollbarOldRemove;var a=function(t,e,r){i.call(this,t,e&&e._rollbar_wrapped||e,r)};a._rollbarOldRemove=i,a.belongsToShim=r,e.removeEventListener=a}}t.exports={captureUncaughtExceptions:r,captureUnhandledRejections:o,wrapGlobals:i}},function(t,e,r){"use strict";function n(t,e,r,n,o){n&&l.isFunction(n)||(n=function(){}),l.addParamsAndAccessTokenToPath(t,e,r);var a="GET",s=l.formatUrl(e);i(t,s,a,null,n,o)}function o(t,e,r,n,o){if(n&&l.isFunction(n)||(n=function(){}),!r)return n(new Error("Cannot send empty request"));var a=p.truncate(r);if(a.error)return n(a.error);var s=a.value,u="POST",c=l.formatUrl(e);i(t,c,u,s,n,o)}function i(t,e,r,n,o,i){var p;if(p=i?i():a(),!p)return o(new Error("No way to send a request"));try{try{var h=function(){try{if(h&&4===p.readyState){h=void 0;var t=l.jsonParse(p.responseText);if(s(p))return void o(t.error,t.value);if(u(p)){if(403===p.status){var e=t.value&&t.value.message;f.error(e)}o(new Error(String(p.status)))}else{var r="XHR response had no status code (likely connection failure)";o(c(r))}}}catch(t){var n;n=t&&t.stack?t:new Error(t),o(n)}};p.open(r,e,!0),p.setRequestHeader&&(p.setRequestHeader("Content-Type","application/json"),p.setRequestHeader("X-Rollbar-Access-Token",t)),p.onreadystatechange=h,p.send(n)}catch(t){if("undefined"!=typeof XDomainRequest){if(!window||!window.location)return o(new Error("No window available during request, unknown environment"));"http:"===window.location.href.substring(0,5)&&"https"===e.substring(0,5)&&(e="http"+e.substring(5));var d=new XDomainRequest;d.onprogress=function(){},d.ontimeout=function(){var t="Request timed out",e="ETIMEDOUT";o(c(t,e))},d.onerror=function(){o(new Error("Error during request"))},d.onload=function(){var t=l.jsonParse(d.responseText);o(t.error,t.value)},d.open(r,e,!0),d.send(n)}else o(new Error("Cannot find a method to transport a request"))}}catch(t){o(t)}}function a(){var t,e,r=[function(){return new XMLHttpRequest},function(){return new ActiveXObject("Msxml2.XMLHTTP")},function(){return new ActiveXObject("Msxml3.XMLHTTP")},function(){return new ActiveXObject("Microsoft.XMLHTTP")}],n=r.length;for(e=0;e<n;e++)try{t=r[e]();break}catch(t){}return t}function s(t){return t&&t.status&&200===t.status}function u(t){return t&&l.isType(t.status,"number")&&t.status>=400&&t.status<600}function c(t,e){var r=new Error(t);return r.code=e||"ENOTFOUND",r}var l=r(5),p=r(18),f=r(13);t.exports={get:n,post:o}},function(t,e,r){"use strict";function n(t,e){return[t,f.stringify(t,e)]}function o(t,e){var r=t.length;return r>2*e?t.slice(0,e).concat(t.slice(r-e)):t}function i(t,e,r){r="undefined"==typeof r?30:r;var n,i=t.data.body;if(i.trace_chain)for(var a=i.trace_chain,s=0;s<a.length;s++)n=a[s].frames,n=o(n,r),a[s].frames=n;else i.trace&&(n=i.trace.frames,n=o(n,r),i.trace.frames=n);return[t,f.stringify(t,e)]}function a(t,e){return e&&e.length>t?e.slice(0,t-3).concat("..."):e}function s(t,e,r){function n(e,r,o){switch(f.typeName(r)){case"string":return a(t,r);case"object":case"array":return f.traverse(r,n,o);default:return r}}return e=f.traverse(e,n,[]),[e,f.stringify(e,r)]}function u(t){return t.exception&&(delete t.exception.description,t.exception.message=a(255,t.exception.message)),t.frames=o(t.frames,1),t}function c(t,e){var r=t.data.body;if(r.trace_chain)for(var n=r.trace_chain,o=0;o<n.length;o++)n[o]=u(n[o]);else r.trace&&(r.trace=u(r.trace));return[t,f.stringify(t,e)]}function l(t,e){return t.length>e}function p(t,e,r){r="undefined"==typeof r?524288:r;for(var o,a,u,p=[n,i,s.bind(null,1024),s.bind(null,512),s.bind(null,256),c];o=p.shift();)if(a=o(t,e),t=a[0],u=a[1],u.error||!l(u.value,r))return u;return u}var f=r(5);t.exports={truncate:p,raw:n,truncateFrames:i,truncateStrings:s,maybeTruncateValue:a}},function(t,e){"use strict";function r(t){var e,r,n={protocol:null,auth:null,host:null,path:null,hash:null,href:t,hostname:null,port:null,pathname:null,search:null,query:null};if(e=t.indexOf("//"),e!==-1?(n.protocol=t.substring(0,e),r=e+2):r=0,e=t.indexOf("@",r),e!==-1&&(n.auth=t.substring(r,e),r=e+1),e=t.indexOf("/",r),e===-1){if(e=t.indexOf("?",r),e===-1)return e=t.indexOf("#",r),e===-1?n.host=t.substring(r):(n.host=t.substring(r,e),n.hash=t.substring(e)),n.hostname=n.host.split(":")[0],n.port=n.host.split(":")[1],n.port&&(n.port=parseInt(n.port,10)),n;n.host=t.substring(r,e),n.hostname=n.host.split(":")[0],n.port=n.host.split(":")[1],n.port&&(n.port=parseInt(n.port,10)),r=e}else n.host=t.substring(r,e),n.hostname=n.host.split(":")[0],n.port=n.host.split(":")[1],n.port&&(n.port=parseInt(n.port,10)),r=e;if(e=t.indexOf("#",r),e===-1?n.path=t.substring(r):(n.path=t.substring(r,e),n.hash=t.substring(e)),n.path){var o=n.path.split("?");n.pathname=o[0],n.query=o[1],n.search=n.query?"?"+n.query:null}return n}t.exports={parse:r}},function(t,e,r){"use strict";function n(t,e,r){if(t.data=t.data||{},t.err)try{t.stackInfo=t.err._savedStackTrace||d.parse(t.err)}catch(e){m.error("Error while parsing the error object.",e);try{t.message=t.err.message||t.err.description||t.message||String(t.err)}catch(e){t.message=String(t.err)||String(e)}delete t.err}r(null,t)}function o(t,e,r){t.message||t.stackInfo||t.custom||r(new Error("No message, stack info, or custom data"),null),r(null,t)}function i(t,e,r){var n=e.payload&&e.payload.environment||e.environment;t.data=h.merge(t.data,{environment:n,level:t.level,endpoint:e.endpoint,platform:"browser",framework:"browser-js",language:"javascript",server:{},uuid:t.uuid,notifier:{name:"rollbar-browser-js",version:e.version}}),r(null,t)}function a(t){return function(e,r,n){if(!t||!t.location)return n(null,e);var o="$remote_ip";r.captureIp?r.captureIp!==!0&&(o+="_anonymize"):o=null,h.set(e,"data.request",{url:t.location.href,query_string:t.location.search,user_ip:o}),n(null,e)}}function s(t){return function(e,r,n){if(!t)return n(null,e);var o=t.navigator||{},i=t.screen||{};h.set(e,"data.client",{runtime_ms:e.timestamp-t._rollbarStartTime,timestamp:Math.round(e.timestamp/1e3),javascript:{browser:o.userAgent,language:o.language,cookie_enabled:o.cookieEnabled,screen:{width:i.width,height:i.height}}}),n(null,e)}}function u(t){return function(e,r,n){if(!t||!t.navigator)return n(null,e);for(var o,i=[],a=t.navigator.plugins||[],s=0,u=a.length;s<u;++s)o=a[s],i.push({name:o.name,description:o.description});h.set(e,"data.client.javascript.plugins",i),n(null,e)}}function c(t,e,r){t.stackInfo?p(t,e,r):l(t,e,r)}function l(t,e,r){var n=t.message,o=t.custom;if(!n)if(o){var i=e.scrubFields,a=h.stringify(h.scrub(o,i));n=a.error||a.value||""}else n="";var s={body:n};o&&(s.extra=h.merge(o)),h.set(t,"data.body",{message:s}),r(null,t)}function p(t,e,r){var n=t.data.description,o=t.stackInfo,i=t.custom,a=d.guessErrorClass(o.message),s=o.name||a[0],u=a[1],c={exception:{class:s,message:u}};n&&(c.exception.description=n);var p=o.stack;if(p&&0===p.length&&t._unhandledStackInfo&&t._unhandledStackInfo.stack&&(p=t._unhandledStackInfo.stack),p){0===p.length&&(c.exception.stack=o.rawStack,c.exception.raw=String(o.rawException));var f,m,g,v,y,b,w,_;for(c.frames=[],w=0;w<p.length;++w)f=p[w],m={filename:f.url?h.sanitizeUrl(f.url):"(unknown)",lineno:f.line||null,method:f.func&&"?"!==f.func?f.func:"[anonymous]",colno:f.column},e.sendFrameUrl&&(m.url=f.url),m.method&&m.method.endsWith&&m.method.endsWith("_rollbar_wrapped")||(g=v=y=null,b=f.context?f.context.length:0,b&&(_=Math.floor(b/2),v=f.context.slice(0,_),g=f.context[_],y=f.context.slice(_)),g&&(m.code=g),(v||y)&&(m.context={},v&&v.length&&(m.context.pre=v),y&&y.length&&(m.context.post=y)),f.args&&(m.args=f.args),c.frames.push(m));c.frames.reverse(),i&&(c.extra=h.merge(i)),h.set(t,"data.body",{trace:c}),r(null,t)}else t.message=s+": "+u,l(t,e,r)}function f(t,e,r){var n=e.scrubFields;t.data=h.scrub(t.data,n),r(null,t)}var h=r(5),d=r(21),m=r(13);t.exports={handleItemWithError:n,ensureItemHasSomethingToSay:o,addBaseInfo:i,addRequestInfo:a,addClientInfo:s,addPluginInfo:u,addBody:c,scrubPayload:f}},function(t,e,r){"use strict";function n(){return l}function o(){return null}function i(t){var e={};return e._stackFrame=t,e.url=t.fileName,e.line=t.lineNumber,e.func=t.functionName,e.column=t.columnNumber,e.args=t.args,e.context=o(),e}function a(t){function e(){var e,r=[];if(t.stack)e=t;else try{throw t}catch(t){e=t}try{r=c.parse(e)}catch(t){r=[]}for(var n=[],o=0;o<r.length;o++)n.push(new i(r[o]));return n}var r=t.constructor&&t.constructor.name;return(!r||!r.length||r.length<3)&&(r=t.name),{stack:e(),message:t.message,name:r,rawStack:t.stack,rawException:t}}function s(t){return new a(t)}function u(t){if(!t||!t.match)return["Unknown error. There was no error message to display.",""];var e=t.match(p),r="(unknown)";return e&&(r=e[e.length-1],t=t.replace((e[e.length-2]||"")+r+":",""),t=t.replace(/(^[\s]+|[\s]+$)/g,"")),[r,t]}var c=r(22),l="?",p=new RegExp("^(([a-zA-Z0-9-_$ ]*): *)?(Uncaught )?([a-zA-Z0-9-_$ ]*): ");t.exports={guessFunctionName:n,guessErrorClass:u,gatherContext:o,parse:s,Stack:a,Frame:i}},function(t,e,r){var n,o,i;!function(a,s){"use strict";o=[r(23)],n=s,i="function"==typeof n?n.apply(e,o):n,!(void 0!==i&&(t.exports=i))}(this,function(t){"use strict";function e(t,e,r){if("function"==typeof Array.prototype.map)return t.map(e,r);for(var n=new Array(t.length),o=0;o<t.length;o++)n[o]=e.call(r,t[o]);return n}function r(t,e,r){if("function"==typeof Array.prototype.filter)return t.filter(e,r);for(var n=[],o=0;o<t.length;o++)e.call(r,t[o])&&n.push(t[o]);return n}var n=/(^|@)\S+\:\d+/,o=/^\s*at .*(\S+\:\d+|\(native\))/m,i=/^(eval@)?(\[native code\])?$/;return{parse:function(t){if("undefined"!=typeof t.stacktrace||"undefined"!=typeof t["opera#sourceloc"])return this.parseOpera(t);if(t.stack&&t.stack.match(o))return this.parseV8OrIE(t);if(t.stack)return this.parseFFOrSafari(t);throw new Error("Cannot parse given Error object")},extractLocation:function(t){if(t.indexOf(":")===-1)return[t];var e=t.replace(/[\(\)\s]/g,"").split(":"),r=e.pop(),n=e[e.length-1];if(!isNaN(parseFloat(n))&&isFinite(n)){var o=e.pop();return[e.join(":"),o,r]}return[e.join(":"),r,void 0]},parseV8OrIE:function(n){var i=r(n.stack.split("\n"),function(t){return!!t.match(o)},this);return e(i,function(e){e.indexOf("(eval ")>-1&&(e=e.replace(/eval code/g,"eval").replace(/(\(eval at [^\()]*)|(\)\,.*$)/g,""));var r=e.replace(/^\s+/,"").replace(/\(eval code/g,"(").split(/\s+/).slice(1),n=this.extractLocation(r.pop()),o=r.join(" ")||void 0,i="eval"===n[0]?void 0:n[0];return new t(o,void 0,i,n[1],n[2],e)},this)},parseFFOrSafari:function(n){var o=r(n.stack.split("\n"),function(t){return!t.match(i)},this);return e(o,function(e){if(e.indexOf(" > eval")>-1&&(e=e.replace(/ line (\d+)(?: > eval line \d+)* > eval\:\d+\:\d+/g,":$1")),e.indexOf("@")===-1&&e.indexOf(":")===-1)return new t(e);var r=e.split("@"),n=this.extractLocation(r.pop()),o=r.shift()||void 0;return new t(o,void 0,n[0],n[1],n[2],e)},this)},parseOpera:function(t){return!t.stacktrace||t.message.indexOf("\n")>-1&&t.message.split("\n").length>t.stacktrace.split("\n").length?this.parseOpera9(t):t.stack?this.parseOpera11(t):this.parseOpera10(t)},parseOpera9:function(e){for(var r=/Line (\d+).*script (?:in )?(\S+)/i,n=e.message.split("\n"),o=[],i=2,a=n.length;i<a;i+=2){var s=r.exec(n[i]);s&&o.push(new t(void 0,void 0,s[2],s[1],void 0,n[i]))}return o},parseOpera10:function(e){for(var r=/Line (\d+).*script (?:in )?(\S+)(?:: In function (\S+))?$/i,n=e.stacktrace.split("\n"),o=[],i=0,a=n.length;i<a;i+=2){var s=r.exec(n[i]);s&&o.push(new t(s[3]||void 0,void 0,s[2],s[1],void 0,n[i]))}return o},parseOpera11:function(o){var i=r(o.stack.split("\n"),function(t){return!!t.match(n)&&!t.match(/^Error created at/)},this);return e(i,function(e){var r,n=e.split("@"),o=this.extractLocation(n.pop()),i=n.shift()||"",a=i.replace(/<anonymous function(: (\w+))?>/,"$2").replace(/\([^\)]*\)/g,"")||void 0;i.match(/\(([^\)]*)\)/)&&(r=i.replace(/^[^\(]+\(([^\)]*)\)$/,"$1"));var s=void 0===r||"[arguments not available]"===r?void 0:r.split(",");return new t(a,s,o[0],o[1],o[2],e)},this)}}})},function(t,e,r){var n,o,i;!function(r,a){"use strict";o=[],n=a,i="function"==typeof n?n.apply(e,o):n,!(void 0!==i&&(t.exports=i))}(this,function(){"use strict";function t(t){return!isNaN(parseFloat(t))&&isFinite(t)}function e(t,e,r,n,o,i){void 0!==t&&this.setFunctionName(t),void 0!==e&&this.setArgs(e),void 0!==r&&this.setFileName(r),void 0!==n&&this.setLineNumber(n),void 0!==o&&this.setColumnNumber(o),void 0!==i&&this.setSource(i)}return e.prototype={getFunctionName:function(){return this.functionName},setFunctionName:function(t){this.functionName=String(t)},getArgs:function(){return this.args},setArgs:function(t){if("[object Array]"!==Object.prototype.toString.call(t))throw new TypeError("Args must be an Array");this.args=t},getFileName:function(){return this.fileName},setFileName:function(t){this.fileName=String(t)},getLineNumber:function(){return this.lineNumber},setLineNumber:function(e){if(!t(e))throw new TypeError("Line Number must be a Number");this.lineNumber=Number(e)},getColumnNumber:function(){return this.columnNumber},setColumnNumber:function(e){if(!t(e))throw new TypeError("Column Number must be a Number");this.columnNumber=Number(e)},getSource:function(){return this.source},setSource:function(t){this.source=String(t)},toString:function(){var e=this.getFunctionName()||"{anonymous}",r="("+(this.getArgs()||[]).join(",")+")",n=this.getFileName()?"@"+this.getFileName():"",o=t(this.getLineNumber())?":"+this.getLineNumber():"",i=t(this.getColumnNumber())?":"+this.getColumnNumber():"";return e+r+n+o+i}},e})},function(t,e,r){"use strict";function n(t,e,r){var n=e.payload||{};n.body&&delete n.body;var o=u.merge(t.data,n);t._isUncaught&&(o._isUncaught=!0),t._originalArgs&&(o._originalArgs=t._originalArgs),r(null,o)}function o(t,e,r){t.telemetryEvents&&u.set(t,"data.body.telemetry",t.telemetryEvents),r(null,t)}function i(t,e,r){if(!t.message)return void r(null,t);var n="data.body.trace_chain.0",o=u.get(t,n);if(o||(n="data.body.trace",o=u.get(t,n)),o){if(!o.exception||!o.exception.description)return u.set(t,n+".exception.description",t.message),void r(null,t);var i=u.get(t,n+".extra")||{},a=u.merge(i,{message:t.message});u.set(t,n+".extra",a)}r(null,t)}function a(t){return function(e,r,n){var o=u.merge(e);try{u.isFunction(r.transform)&&r.transform(o.data,e)}catch(o){return r.transform=null,t.error("Error while calling custom transform() function. Removing custom transform().",o),void n(null,e)}n(null,o)}}function s(t,e,r){if(!e.sendConfig)return r(null,t);var n="_rollbarConfig",o=u.get(t,"data.custom")||{};o[n]=e,t.data.custom=o,r(null,t)}var u=r(5);t.exports={itemToPayload:n,addTelemetryData:o,addMessageWithError:i,userTransform:a,addConfigToPayload:s}},function(t,e,r){"use strict";function n(t,e){return!o.get(e,"plugins.jquery.ignoreAjaxErrors")||!o.get(t,"body.message.extra.isAjax")}var o=r(5);t.exports={checkIgnore:n}},function(t,e,r){"use strict";function n(t,e){var r=t.level,n=c.LEVELS[r]||0,o=e.reportLevel,i=c.LEVELS[o]||0;return!(n<i)}function o(t){return function(e,r){var n=!!e._isUncaught;delete e._isUncaught;var o=e._originalArgs;delete e._originalArgs;try{c.isFunction(r.onSendCallback)&&r.onSendCallback(n,o,e)}catch(e){r.onSendCallback=null,t.error("Error while calling onSendCallback, removing",e)}try{if(c.isFunction(r.checkIgnore)&&r.checkIgnore(n,o,e))return!1}catch(e){r.checkIgnore=null,t.error("Error while calling custom checkIgnore(), removing",e)}return!0}}function i(t){return function(e,r){return!s(e,r,"blacklist",t)}}function a(t){return function(e,r){return s(e,r,"whitelist",t)}}function s(t,e,r,n){var o=!1;"blacklist"===r&&(o=!0);var i,a,s,u,l,p,f,h,d,m;try{if(i=o?e.hostBlackList:e.hostWhiteList,f=i&&i.length,a=c.get(t,"body.trace"),!i||0===f)return!o;if(!a||!a.frames||0===a.frames.length)return!o;for(l=a.frames.length,d=0;d<l;d++){if(s=a.frames[d],u=s.filename,!c.isType(u,"string"))return!o;for(m=0;m<f;m++)if(p=i[m],h=new RegExp(p),h.test(u))return!0}}catch(t){o?e.hostBlackList=null:e.hostWhiteList=null;var g=o?"hostBlackList":"hostWhiteList";return n.error("Error while reading your configuration's "+g+" option. Removing custom "+g+".",t),!o}return!1}function u(t){return function(e,r){var n,o,i,a,s,u,l,p,f;try{if(s=!1,i=r.ignoredMessages,!i||0===i.length)return!0;if(l=e.body,p=c.get(l,"trace.exception.message"),f=c.get(l,"message.body"),n=p||f,!n)return!0;for(a=i.length,o=0;o<a&&(u=new RegExp(i[o],"gi"),!(s=u.test(n)));o++);}catch(e){r.ignoredMessages=null,t.error("Error while reading your configuration's ignoredMessages option. Removing custom ignoredMessages.")}return!s}}var c=r(5);t.exports={checkLevel:n,userCheckIgnore:o,urlIsNotBlacklisted:i,urlIsWhitelisted:a,messageIsIgnored:u}},function(t,e,r){"use strict";function n(t,e,r,n,o){var i=t[e];t[e]=r(i),n&&n[o].push([t,e,i])}function o(t,e){for(var r;t[e].length;)r=t[e].shift(),r[0][r[1]]=r[2]}function i(t){if(!t||!t.attributes)return null;for(var e=t.attributes,r=0;r<e.length;++r)if("name"===e[r].key)return e[r].value;return null}function a(t){for(var e=[],r=0;r<t.length;++r)e.push(new RegExp(t[r],"i"));return function(t){var r=i(t);if(!r)return!1;for(var n=0;n<e.length;++n)if(e[n].test(r))return!0;return!1}}function s(t,e,r,n,o){var i=t.autoInstrument;t.enabled===!1||i===!1?this.autoInstrument={}:(u.isType(i,"object")||(i=p),this.autoInstrument=u.merge(p,i)),this.scrubTelemetryInputs=!!t.scrubTelemetryInputs,this.telemetryScrubber=t.telemetryScrubber,this.defaultValueScrubber=a(t.scrubFields),this.telemeter=e,this.rollbar=r,this._window=n||{},this._document=o||{},this.replacements={network:[],log:[],navigation:[],connectivity:[]},this.eventRemovers={dom:[],connectivity:[]},this._location=this._window.location,this._lastHref=this._location&&this._location.href}var u=r(5),c=r(19),l=r(28),p={network:!0,networkResponseHeaders:!1,networkResponseBody:!1,networkRequestBody:!1,log:!0,dom:!0,navigation:!0,connectivity:!0};s.prototype.configure=function(t){var e=t.autoInstrument,r=u.merge(this.autoInstrument);t.enabled===!1||e===!1?this.autoInstrument={}:(u.isType(e,"object")||(e=p),this.autoInstrument=u.merge(p,e)),this.instrument(r),void 0!==t.scrubTelemetryInputs&&(this.scrubTelemetryInputs=!!t.scrubTelemetryInputs),void 0!==t.telemetryScrubber&&(this.telemetryScrubber=t.telemetryScrubber)},s.prototype.instrument=function(t){!this.autoInstrument.network||t&&t.network?!this.autoInstrument.network&&t&&t.network&&this.deinstrumentNetwork():this.instrumentNetwork(),!this.autoInstrument.log||t&&t.log?!this.autoInstrument.log&&t&&t.log&&this.deinstrumentConsole():this.instrumentConsole(),!this.autoInstrument.dom||t&&t.dom?!this.autoInstrument.dom&&t&&t.dom&&this.deinstrumentDom():this.instrumentDom(),!this.autoInstrument.navigation||t&&t.navigation?!this.autoInstrument.navigation&&t&&t.navigation&&this.deinstrumentNavigation():this.instrumentNavigation(),!this.autoInstrument.connectivity||t&&t.connectivity?!this.autoInstrument.connectivity&&t&&t.connectivity&&this.deinstrumentConnectivity():this.instrumentConnectivity()},s.prototype.deinstrumentNetwork=function(){o(this.replacements,"network")},s.prototype.instrumentNetwork=function(){function t(t,r){t in r&&u.isFunction(r[t])&&n(r,t,function(t){return e.rollbar.wrap(t)})}var e=this;if("XMLHttpRequest"in this._window){var r=this._window.XMLHttpRequest.prototype;n(r,"open",function(t){return function(e,r){return u.isType(r,"string")&&(this.__rollbar_xhr={method:e,url:r,status_code:null,start_time_ms:u.now(),end_time_ms:null}),t.apply(this,arguments)}},this.replacements,"network"),n(r,"send",function(r){return function(o){function i(){if(a.__rollbar_xhr){if(null===a.__rollbar_xhr.status_code){a.__rollbar_xhr.status_code=0;var t=null;e.autoInstrument.networkRequestBody&&(t=o),a.__rollbar_event=e.telemeter.captureNetwork(a.__rollbar_xhr,"xhr",void 0,t)}if(a.readyState<2&&(a.__rollbar_xhr.start_time_ms=u.now()),a.readyState>3){a.__rollbar_xhr.end_time_ms=u.now();var r=null;if(e.autoInstrument.networkResponseHeaders){var n=e.autoInstrument.networkResponseHeaders;r={};try{var i,s;if(n===!0){var c=a.getAllResponseHeaders();if(c){var l,p,f=c.trim().split(/[\r\n]+/);for(s=0;s<f.length;s++)l=f[s].split(": "),i=l.shift(),p=l.join(": "),r[i]=p}}else for(s=0;s<n.length;s++)i=n[s],r[i]=a.getResponseHeader(i)}catch(t){}}var h=null;if(e.autoInstrument.networkResponseBody)try{h=a.responseText}catch(t){}var d=null;(h||r)&&(d={},h&&(d.body=h),r&&(d.headers=r)),d&&(a.__rollbar_xhr.response=d);try{var m=a.status;m=1223===m?204:m,a.__rollbar_xhr.status_code=m,a.__rollbar_event.level=e.telemeter.levelFromStatus(m)}catch(t){}}}}var a=this;return t("onload",a),t("onerror",a),t("onprogress",a),"onreadystatechange"in a&&u.isFunction(a.onreadystatechange)?n(a,"onreadystatechange",function(t){return e.rollbar.wrap(t,void 0,i)}):a.onreadystatechange=i,r.apply(this,arguments)}},this.replacements,"network")}"fetch"in this._window&&n(this._window,"fetch",function(t){return function(r,n){for(var o=new Array(arguments.length),i=0,a=o.length;i<a;i++)o[i]=arguments[i];var s,c=o[0],l="GET";u.isType(c,"string")?s=c:c&&(s=c.url,c.method&&(l=c.method)),o[1]&&o[1].method&&(l=o[1].method);var p={method:l,url:s,status_code:null,start_time_ms:u.now(),end_time_ms:null},f=null;return e.autoInstrument.networkRequestBody&&(o[1]&&o[1].body?f=o[1].body:o[0]&&!u.isType(o[0],"string")&&o[0].body&&(f=o[0].body)),e.telemeter.captureNetwork(p,"fetch",void 0,f),t.apply(this,o).then(function(t){p.end_time_ms=u.now(),p.status_code=t.status;var r=null;if(e.autoInstrument.networkResponseHeaders){var n=e.autoInstrument.networkResponseHeaders;r={};try{if(n===!0);else for(var o=0;o<n.length;o++){var i=n[o];r[i]=t.headers.get(i)}}catch(t){}}var a=null;return r&&(a={headers:r}),a&&(p.response=a),t})}},this.replacements,"network")},s.prototype.deinstrumentConsole=function(){if("console"in this._window&&this._window.console.log)for(var t;this.replacements.log.length;)t=this.replacements.log.shift(),this._window.console[t[0]]=t[1]},s.prototype.instrumentConsole=function(){function t(t){var n=r[t],o=r,i="warn"===t?"warning":t;r[t]=function(){var t=Array.prototype.slice.call(arguments),r=u.formatArgsAsString(t);e.telemeter.captureLog(r,i),n&&Function.prototype.apply.call(n,o,t)},e.replacements.log.push([t,n])}if("console"in this._window&&this._window.console.log)for(var e=this,r=this._window.console,n=["debug","info","warn","error","log"],o=0,i=n.length;o<i;o++)t(n[o])},s.prototype.deinstrumentDom=function(){("addEventListener"in this._window||"attachEvent"in this._window)&&this.removeListeners("dom")},s.prototype.instrumentDom=function(){if("addEventListener"in this._window||"attachEvent"in this._window){var t=this.handleClick.bind(this),e=this.handleBlur.bind(this);this.addListener("dom",this._window,"click","onclick",t,!0),this.addListener("dom",this._window,"blur","onfocusout",e,!0)}},s.prototype.handleClick=function(t){try{var e=l.getElementFromEvent(t,this._document),r=e&&e.tagName,n=l.isDescribedElement(e,"a")||l.isDescribedElement(e,"button");r&&(n||l.isDescribedElement(e,"input",["button","submit"]))?this.captureDomEvent("click",e):l.isDescribedElement(e,"input",["checkbox","radio"])&&this.captureDomEvent("input",e,e.value,e.checked)}catch(t){}},s.prototype.handleBlur=function(t){try{var e=l.getElementFromEvent(t,this._document);e&&e.tagName&&(l.isDescribedElement(e,"textarea")?this.captureDomEvent("input",e,e.value):l.isDescribedElement(e,"select")&&e.options&&e.options.length?this.handleSelectInputChanged(e):l.isDescribedElement(e,"input")&&!l.isDescribedElement(e,"input",["button","submit","hidden","checkbox","radio"])&&this.captureDomEvent("input",e,e.value))}catch(t){}},s.prototype.handleSelectInputChanged=function(t){if(t.multiple)for(var e=0;e<t.options.length;e++)t.options[e].selected&&this.captureDomEvent("input",t,t.options[e].value);else t.selectedIndex>=0&&t.options[t.selectedIndex]&&this.captureDomEvent("input",t,t.options[t.selectedIndex].value)},s.prototype.captureDomEvent=function(t,e,r,n){if(void 0!==r)if(this.scrubTelemetryInputs||"password"===l.getElementType(e))r="[scrubbed]";else{var o=l.describeElement(e);this.telemetryScrubber?this.telemetryScrubber(o)&&(r="[scrubbed]"):this.defaultValueScrubber(o)&&(r="[scrubbed]")}var i=l.elementArrayToString(l.treeToArray(e));this.telemeter.captureDom(t,i,r,n)},s.prototype.deinstrumentNavigation=function(){var t=this._window.chrome,e=t&&t.app&&t.app.runtime,r=!e&&this._window.history&&this._window.history.pushState;r&&o(this.replacements,"navigation")},s.prototype.instrumentNavigation=function(){var t=this._window.chrome,e=t&&t.app&&t.app.runtime,r=!e&&this._window.history&&this._window.history.pushState;if(r){var o=this;n(this._window,"onpopstate",function(t){return function(){var e=o._location.href;o.handleUrlChange(o._lastHref,e),t&&t.apply(this,arguments)}},this.replacements,"navigation"),n(this._window.history,"pushState",function(t){return function(){var e=arguments.length>2?arguments[2]:void 0;return e&&o.handleUrlChange(o._lastHref,e+""),t.apply(this,arguments)}},this.replacements,"navigation")}},s.prototype.handleUrlChange=function(t,e){var r=c.parse(this._location.href),n=c.parse(e),o=c.parse(t);this._lastHref=e,r.protocol===n.protocol&&r.host===n.host&&(e=n.path+(n.hash||"")),r.protocol===o.protocol&&r.host===o.host&&(t=o.path+(o.hash||"")),this.telemeter.captureNavigation(t,e)},s.prototype.deinstrumentConnectivity=function(){("addEventListener"in this._window||"body"in this._document)&&(this._window.addEventListener?this.removeListeners("connectivity"):o(this.replacements,"connectivity"))},s.prototype.instrumentConnectivity=function(){if("addEventListener"in this._window||"body"in this._document)if(this._window.addEventListener)this.addListener("connectivity",this._window,"online",void 0,function(){this.telemeter.captureConnectivityChange("online")}.bind(this),!0),this.addListener("connectivity",this._window,"offline",void 0,function(){this.telemeter.captureConnectivityChange("offline")}.bind(this),!0);else{var t=this;n(this._document.body,"ononline",function(e){return function(){t.telemeter.captureConnectivityChange("online"),e&&e.apply(this,arguments)}},this.replacements,"connectivity"),n(this._document.body,"onoffline",function(e){return function(){t.telemeter.captureConnectivityChange("offline"),e&&e.apply(this,arguments)}},this.replacements,"connectivity")}},s.prototype.addListener=function(t,e,r,n,o,i){e.addEventListener?(e.addEventListener(r,o,i),this.eventRemovers[t].push(function(){e.removeEventListener(r,o,i)})):n&&(e.attachEvent(n,o),this.eventRemovers[t].push(function(){e.detachEvent(n,o)}))},s.prototype.removeListeners=function(t){for(var e;this.eventRemovers[t].length;)(e=this.eventRemovers[t].shift())()},t.exports=s},function(t,e){"use strict";function r(t){return(t.getAttribute("type")||"").toLowerCase()}function n(t,e,n){if(t.tagName.toLowerCase()!==e.toLowerCase())return!1;if(!n)return!0;t=r(t);for(var o=0;o<n.length;o++)if(n[o]===t)return!0;return!1}function o(t,e){return t.target?t.target:e&&e.elementFromPoint?e.elementFromPoint(t.clientX,t.clientY):void 0}function i(t){for(var e,r=5,n=[],o=0;t&&o<r&&(e=u(t),"html"!==e.tagName);o++)n.unshift(e),t=t.parentNode;return n}function a(t){for(var e,r,n=80,o=" > ",i=o.length,a=[],u=0,c=t.length-1;c>=0;c--){if(e=s(t[c]),r=u+a.length*i+e.length,c<t.length-1&&r>=n+3){a.unshift("...");break}a.unshift(e),u+=e.length}return a.join(o)}function s(t){if(!t||!t.tagName)return"";var e=[t.tagName];t.id&&e.push("#"+t.id),t.classes&&e.push("."+t.classes.join("."));for(var r=0;r<t.attributes.length;r++)e.push("["+t.attributes[r].key+'="'+t.attributes[r].value+'"]');return e.join("")}function u(t){if(!t||!t.tagName)return null;var e,r,n,o,i={};i.tagName=t.tagName.toLowerCase(),t.id&&(i.id=t.id),e=t.className,e&&"string"==typeof e&&(i.classes=e.split(/\s+/));var a=["type","name","title","alt"];for(i.attributes=[],o=0;o<a.length;o++)r=a[o],n=t.getAttribute(r),n&&i.attributes.push({key:r,value:n});return i}t.exports={describeElement:u,descriptionToString:s,elementArrayToString:a,treeToArray:i,getElementFromEvent:o,isDescribedElement:n,getElementType:r}}])});

},{}],5:[function(require,module,exports){
'use strict'

var parseUnit = require('parse-unit')

module.exports = toPX

var PIXELS_PER_INCH = getSizeBrutal('in', document.body) // 96


function getPropertyInPX(element, prop) {
  var parts = parseUnit(getComputedStyle(element).getPropertyValue(prop))
  return parts[0] * toPX(parts[1], element)
}

//This brutal hack is needed
function getSizeBrutal(unit, element) {
  var testDIV = document.createElement('div')
  testDIV.style['height'] = '128' + unit
  element.appendChild(testDIV)
  var size = getPropertyInPX(testDIV, 'height') / 128
  element.removeChild(testDIV)
  return size
}

function toPX(str, element) {
  if (!str) return null

  element = element || document.body
  str = (str + '' || 'px').trim().toLowerCase()
  if(element === window || element === document) {
    element = document.body
  }

  switch(str) {
    case '%':  //Ambiguous, not sure if we should use width or height
      return element.clientHeight / 100.0
    case 'ch':
    case 'ex':
      return getSizeBrutal(str, element)
    case 'em':
      return getPropertyInPX(element, 'font-size')
    case 'rem':
      return getPropertyInPX(document.body, 'font-size')
    case 'vw':
      return window.innerWidth/100
    case 'vh':
      return window.innerHeight/100
    case 'vmin':
      return Math.min(window.innerWidth, window.innerHeight) / 100
    case 'vmax':
      return Math.max(window.innerWidth, window.innerHeight) / 100
    case 'in':
      return PIXELS_PER_INCH
    case 'cm':
      return PIXELS_PER_INCH / 2.54
    case 'mm':
      return PIXELS_PER_INCH / 25.4
    case 'pt':
      return PIXELS_PER_INCH / 72
    case 'pc':
      return PIXELS_PER_INCH / 6
    case 'px':
      return 1
  }

  // detect number of units
  var parts = parseUnit(str)
  if (!isNaN(parts[0]) && parts[1]) {
    var px = toPX(parts[1], element)
    return typeof px === 'number' ? parts[0] * px : null
  }

  return null
}

},{"parse-unit":3}],6:[function(require,module,exports){
document.title = window.location.hostname.split('.')[0] + " - Dark";

class RopArrow extends HTMLElement {
  constructor() {
    super();
  }
  update() {
    var tlid = this.getAttribute("tlid");
    var target = document.querySelector(".tl-" + tlid + " .rop-rail");
    if(target) {
      const targetPos = target.getBoundingClientRect();
      const sourcePos = this.getBoundingClientRect();
      const arrowHeadLength = 12;
      const x2 = targetPos.right - sourcePos.left - arrowHeadLength;
      const x1 = x2 / 3;

      var arrowRight = document.querySelector("rop-arrow > svg > path");
      var svg = document.querySelector("rop-arrow > svg");
      svg.setAttribute("width", x2 + arrowHeadLength);

      var d = "M 0,20 C 0,8 " + x1 + ",8 " + x2 + ",15";
      arrowRight.setAttribute("d", d);
    }
  }
  connectedCallback() {
    this.update();
  }
  static get observedAttributes() {
    return ['update'];
  }
  attributeChangedCallback(attr, _, val) {
    this.update();
  }
}
window.customElements.define('rop-arrow', RopArrow);

const mousewheel = function(callback){
  require("domready")(function () {
    require("mouse-wheel")(document.body, callback);
  });
};

// Allows us capture certain keys and stop them from affecting the browser.
function stopKeys(event) {
  if (event.keyCode == 9) { // Tab
    event.preventDefault();
  }
  if (event.keyCode == 32 // Space
      && !event.target.parentNode.className.includes("string-container")) {
    event.preventDefault();
  }
  if (event.keyCode == 13 // Enter
      && !event.target.parentNode.className.includes("large-string")) {
    event.preventDefault();
  }
  if (event.keyCode == 38 // Up
      || event.keyCode == 40) {  // Down
    if (document.activeElement.tagName.toLowerCase() !== 'textarea')
      event.preventDefault();
  }
}
window.stopKeys = stopKeys;

// ---------------------------
// Rollbar
// ---------------------------
var rollbar = require('rollbar');

var Rollbar = rollbar.init({});
window.Rollbar = Rollbar;
window.Rollbar.configure(rollbarConfig);

var pageHidden = false;

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
  displayError(`Error while executing (${route}): ${error}`);
};

window.Dark = {
  analysis: {
    requestAnalysis : function (params) {
      // console.debug('request analysis');
      // console.debug(params);

      // const bToString = (blankOr) => blankOr[2] || null;
      // const spec = params.handler.spec;
      // const route = `${bToString(spec.module)}, ${bToString(spec.name)}, ${bToString(spec.modifier)}`;

      if (window.analysisWorker) {
        window.analysisWorker.postMessage(
          { proto: window.location.protocol,
            params: params
          }
        );

        window.analysisWorker.onmessage = function (e) {
          var result = e.data.analysis;
          var error = e.data.error;

          if (result && !error) {
            var event = new CustomEvent('receiveAnalysis', {detail: result});
            document.dispatchEvent(event);
          } else if (error) {
            sendError(error);
          }
        }
      } else {
        console.log("analysisworker not loaded yet");
      }
    }
  },
  ast: {
    positions: function (tlid) {
      var extractId = function (elem) {
        var className = elem.className;
        var matches = /.*id-(\S+).*/g.exec(className);
        var id = matches[1];

        if (typeof id === 'undefined')
          throw 'Dark.ast.atomPositions: Cannot match Blank(id) regex for '+className;

        return id;
      };

      var find = function (tl, nested) {
        var atoms = [];
        tl.querySelectorAll(nested ? '.blankOr.nested' : '.blankOr:not(.nested)')
        .forEach((v,i,l) => {
          var rect = v.getBoundingClientRect();
          atoms.push({
            id: extractId(v),
            left: "" + (rect.left | 0),
            right: "" + (rect.right | 0),
            top: "" + (rect.top | 0),
            bottom: "" + (rect.bottom | 0)
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

function displayError (msg){
  var event = new CustomEvent('displayError', {detail: msg});
  document.dispatchEvent(event);
}

function windowFocusChange (visible){
  var event = new CustomEvent('windowFocusChange', {detail: visible});
  document.dispatchEvent(event);
}

window.onerror = function (msg, url, line, col, error) {
  window.Rollbar.error(msg, error);
  displayError(msg);
};


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

function addWheelListener(elem){
  var prefix = "";
  var _addEventListener;
  var support;

  // detect event model
  if ( window.addEventListener ) {
      _addEventListener = "addEventListener";
  } else {
      _addEventListener = "attachEvent";
      prefix = "on";
  }

  // detect available wheel event
  support = "onwheel" in document.createElement("div") ? "wheel" : // Modern browsers support "wheel"
            document.onmousewheel !== undefined ? "mousewheel" : // Webkit and IE support at least "mousewheel"
            "DOMMouseScroll"; // let's assume that remaining browsers are older Firefox

  var listener = function( elem, useCapture ) {
      _addWheelListener( elem, support, useCapture );

      // handle MozMousePixelScroll in older Firefox
      if( support == "DOMMouseScroll" ) {
          _addWheelListener( elem, "MozMousePixelScroll", useCapture );
      }
  };

  function _addWheelListener( elem, eventName, useCapture ) {
      elem[ _addEventListener ](prefix + eventName, function( originalEvent ) {
          !originalEvent && ( originalEvent = window.event );

          // create a normalized event object
          var event = {
              // keep a ref to the original event object
              originalEvent: originalEvent,
              target: originalEvent.target || originalEvent.srcElement,
              type: "wheel",
              deltaMode: originalEvent.type == "MozMousePixelScroll" ? 0 : 1,
              deltaX: 0,
              deltaY: 0,
              deltaZ: 0,
              preventDefault: function() {
                  originalEvent.preventDefault ?
                      originalEvent.preventDefault() :
                      originalEvent.returnValue = false;
              }
          };

          // calculate deltaY (and deltaX) according to the event
          if ( support == "mousewheel" ) {
              event.deltaY = - 1/40 * originalEvent.wheelDelta;
              // Webkit also support wheelDeltaX
              originalEvent.wheelDeltaX && ( event.deltaX = - 1/40 * originalEvent.wheelDeltaX );
          } else {
              event.deltaY = originalEvent.deltaY || originalEvent.detail;
          }

      }, useCapture || false );
  }

  return listener(elem);
}

setTimeout(function(){
  const canvasName = new URL(window.location).pathname.split("/")[2];
  const params = JSON.stringify(
    {
      editorState: window.localStorage.getItem('editorState-' + canvasName),
      complete: complete,
      userContentHost: userContentHost,
      environment: environmentName,
      csrfToken: csrfToken
    });
  var urlParams = new URLSearchParams(window.location.search);
  if (urlParams.has('debug')) {
    app = buckle.debugging(document.body, params);
  } else {
    app = buckle.normal(document.body, params);
  }

  window.onresize = function(evt){
    const size = {
      width : window.innerWidth,
      height: window.innerHeight
    }
    var event = new CustomEvent('windowResize',
      { detail : size })
    document.dispatchEvent(event)
  };

  let darkjsbc  = fetch("//" + staticUrl + "/darkjs.bc.js").then(r => r.text());
  let darkjsexe = fetch("//" + staticUrl + "/darkex.js").then(r => r.text());
  var analysisWorkerUrl;
  (async function () {
    analysisWorkerUrl = window.URL.createObjectURL(
      new Blob(
        [ await darkjsbc
        , "\n\n"
        , await darkjsexe
        ]));
    window.analysisWorker = new Worker(analysisWorkerUrl);
  })();

  window.onfocus = function(evt){ windowFocusChange(true) };
  window.onblur = function(evt){ windowFocusChange(false) };
  setInterval(visibilityCheck, 2000);
  addWheelListener(document);

}, 1)
// ---------------------------
// Exports
// ---------------------------
module.exports = {
  mousewheel: mousewheel
};

},{"domready":1,"mouse-wheel":2,"rollbar":4}]},{},[6])(6)
});
