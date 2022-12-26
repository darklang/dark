

// ---------------------------
// If you need to deploy app.js again with a different sha, increment this number
// Cache break: 1
// ---------------------------

// ---------------------------
// Check unsupported browser
// ---------------------------

open Webapi.Dom
 let unsupportedBrowser = ()=>{
  let navigatorCopy: Webapi.Navigator.t = window->Window.navigator
  let isChrome = Belt.Option.isSome(Js.String.match_(%re("/Chrome/"), navigatorCopy.userAgent)) && Belt.Option.isSome(Js.String.match_(%re("/Google Inc/"),navigatorCopy.vendor))
  let isMobile = Belt.Option.isSome(Js.String.match_(%re("/Android|BlackBerry|iPhone|iPad|iPod|Opera Mini|IEMobile/"), navigatorCopy.userAgent))
  let isDesktop = Belt.Option.isSome(Js.String.match_(%re("/DarkLang\/Editor/"), navigatorCopy.userAgent))
  let isSupported = isDesktop || (isChrome && !isMobile);
   !isSupported
 }


 // This requires being loaded by defer, not as async
open EventTarget
let target = Webapi.Dom.document |> Webapi.Dom.Document.asEventTarget
target->addEventListener("DOMContentLoaded", _ => {
  if (unsupportedBrowser()) {
    let htmlString =
      "<div class='modal-overlay' id='unsupportedBrowser'><div class='modal'><div class='warning'><p class='title'>Dark is only fully supported on Desktop Chrome right now. We have a desktop client you can use if you prefer.</p><p class='title'>Unfortunately, as a small team, we don’t have the capacity to support other browsers at the moment. Once we’ve made the experience excellent on Chrome, we'll address cross-browser compatibility issues to support Firefox, Safari, and mobile devices.</p><p class='title'>Thanks for understanding ❤️</p></div></div></div>";
    let div =  document->Document.createElement("div");
    div->Element.setInnerHTML(htmlString)
    let body =
      document
      ->Document.asHtmlDocument
      ->Belt.Option.flatMap(HtmlDocument.body)
      ->Belt.Option.getUnsafe
    body->Element.appendChild(~child=div)
  }
})


if (unsupportedBrowser()) {
  open MutationRecord
  // Reload page if user tries to delete overlay
  window->Window.addEventListener("DOMContentLoaded", _ => {
    let reload = ref(false)
    let observer= MutationObserver.make((_records, _observer) => ())
    let records: array<Dom.mutationRecord> = observer->MutationObserver.takeRecords
    records|>Js.Array.forEach(record=>{
      record->removedNodes
      |> Webapi.Dom.NodeList.toArray
      |> Js.Array.forEach(node=>{
        Js.log(node)
        // if (node["id"]== "unsupportedBrowser") {
        //   reload := true
        //   }
      })
      if (reload.contents){
          Webapi.Dom.location->Webapi.Dom.Location.reload}
    })
    observer->MutationObserver.observe(
      document->Document.documentElement,
      {
        "subtree": true,
        "childList": true,
        "attributes": true,
        "attributeOldValue": true,
        "characterData": true,
      },
    )
    }
  )}




// ---------------------------
// Analytics
// ---------------------------


// ---------------------------
// Allows us capture certain keys and stop them from affecting the browser.
// ---------------------------



let stopKeys =  event =>{
  open! KeyboardEvent
  // Don't ever attempt to save the HTML of the page.
 if (ctrlKey(event)|| metaKey(event)) && key(event)=="s"{
  event -> preventDefault
 }

 // `Ctrl-K` is meant to open the omnibox on Linux, but without this preventDefault
 // it will focus the browser's URL bar after creating the omnibox.

  if ctrlKey(event)&& key(event)=="k" && Entry.getBrowserPlatform() == Linux{
      event -> preventDefault
  }
}


// ---------------------------
// Is it chrome?
// ---------------------------

let getBrowserPlatform = () => {
  // Checks if mac
  let isMac = Entry.getBrowserPlatform() == Mac
  // Check if Linux
  let isLinux = Entry.getBrowserPlatform() == Linux
  // Check if Windows
  let isWindows = Entry.getBrowserPlatform() == Windows
  // known platform
  if (isMac) {
    0
  } else if (isLinux) {
    1
  } else if (isWindows) {
    2
  } else {
    3
  }
}

// ---------------------------
// Rollbar
// ---------------------------

module Rollbar = {
  type person ={
    id: string,
    username: string,
  }

  type rollbarConfig = {
    enabled: bool,
    payload: person,
  }
  type rollbar =
  {
  init: rollbarConfig => unit,
  error: Js.Json.t => unit,
  configure: rollbarConfig => unit,
  }
  @module external rollbar: rollbar = "rollbar"
  let init = (rollbarConfig: rollbarConfig) => {
    rollbar.init(rollbarConfig)
  }
  let error = (error: Js.Json.t) => {
    rollbar.error(error)
  }
  let payload = (payload: person) => {
    rollbar.configure({enabled: true, payload: payload})
      }
  let enabled = (enabled: bool) => {
    rollbar.configure({enabled: enabled, payload: {id: "", username: ""}})
  }
  let configure = ({enabled: enabled, payload: payload}) => {
    rollbar.configure({enabled: enabled, payload: payload})
  }

let username=""
let userID=""
  let rollbarConfig = {
    enabled: true,
    payload: { id: userID, username: username }
  }
let rollbarg = rollbar.init(rollbarConfig)

let searchParams = Webapi.Url.URLSearchParams.make(window->Window.location->Location.href)
if (searchParams->Webapi.Url.URLSearchParams.get("use-assets-tunnel") != None) {
  rollbar.configure({...rollbarConfig, enabled: false})
}


let displayError = (msg)=>{
  let event = CustomEvent.makeWithOptions(
    "displayError",
    {
      "detail": {
        "detail": msg,
      },
    },
  )
  document->Document.dispatchEvent(event)
}

}

// ---------------------------
// Pusher
// ---------------------------


module Pusher = {
  type pusherConfig = {
    cluster: string,
    key: string,
    forceTLS: bool,
    enabled: bool,

  }
  type pusher = {
    init: pusherConfig => unit,
  }

@module("pusher-js") external pusher: pusher = "Pusher"

let pusherConfig = {
  key: "",
  cluster: "",
  forceTLS: true,
  enabled: true,
}


 if (pusherConfig.enabled) {
  let pusherConnection = pusher.init(pusherConfig)
  pusherConnection

}
}


//window.Dark{}



// ---------------------------
// Focus
// ---------------------------

// mozhidden is not found in webapi

let windowFocusChange = (visible) => {
  let event = CustomEvent.makeWithOptions("windowFocusChange", { "detail": visible });
  let target = Webapi.Dom.document |> Webapi.Dom.Document.asEventTarget
  target->dispatchEvent(event);
}

let pageHidden = ref(false)

let visibilityCheck=() => {
  let hidden = ref(false)
  if (Js.typeof(Webapi.Dom.document |> Document.hidden) !== "undefined") {
    hidden := Webapi.Dom.document |> Document.hidden
  }
  // else if (typeof document.mozHidden !== "undefined") {
  //   hidden = document.mozHidden
  // } else if (typeof document.msHidden !== "undefined") {
  //   hidden = document.msHidden
  // } else if (typeof document.webkitHidden !== "undefined") {
  //   hidden = document.webkitHidden
  // }

  if (pageHidden != hidden) {
    let _ =windowFocusChange(hidden.contents)
    pageHidden := hidden.contents
  }
}


// ---------------------------
// Wheel
// ---------------------------



// ---------------------------
// Load webworkers
// ---------------------------


// ---------------------------
// Initialize blazorworker
// ---------------------------


// ---------------------------
// Detect window focus change
// ---------------------------



// ---------------------------
// Wheel
// ---------------------------


// ---------------------------
// Fullstory
// ---------------------------


// ---------------------------
// Pusher channels
// ---------------------------











