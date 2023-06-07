// This file is to inform Vue of properties on globals that it doesn't know about.

interface Window {
  /**
   * Exposed by `editor-bootstrap.js
   *
   * avails an `init` function to load the Darklang WASM runtime.
   * */
  Darklang: any

  /**
   * The interface with the Darkland WASM runtime.
   *
   * includes functions like `handleEvent`.
   */
  darklang: any

  /** Function exposed to Darklang runtime to recieve updated state from Dark code. */
  stateUpdated: any
}
