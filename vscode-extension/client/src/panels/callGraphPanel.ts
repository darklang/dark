import * as vscode from "vscode";

interface CallNode {
  name: string;
  signature?: string;
  docstring?: string;
  definition?: string;
  calls: CallNode[];
}

// Fake data for proof of concept - based on real Darklang CLI code
const FAKE_CALL_GRAPH: Record<string, CallNode> = {
  "Darklang.Cli.Core.getCompletions": {
    name: "Darklang.Cli.Core.getCompletions",
    signature: "(state: AppState, input: String) -> List<String>",
    docstring: "Returns completion suggestions for CLI input - either command names or command arguments",
    definition: `let getCompletions (state: AppState) (input: String) : List<String> =
  let parsed = Completion.parseInput input

  if parsed.isCompletingCommand then
    // Completing command name
    let partial = Completion.getPartialCompletion parsed
    let commands = allCommands ()
    let allNames = Stdlib.List.map commands (fun cmd -> cmd.name)
    let allAliases = Stdlib.List.fold commands [] (fun acc cmd -> Stdlib.List.append acc cmd.aliases)
    let allOptions = Stdlib.List.append allNames allAliases
    if Stdlib.String.isEmpty partial then
      allOptions
    else
      Stdlib.List.filter allOptions (fun name -> Stdlib.String.startsWith name partial)
  else
    // Completing command arguments
    let handler = findCommand parsed.commandName
    let completeFunc = handler.complete
    completeFunc state parsed.args`,
    calls: [
      {
        name: "Darklang.Cli.Completion.parseInput",
        signature: "(input: String) -> ParsedInput",
        docstring: "Parses raw CLI input into structured completion context",
        definition: `let parseInput (input: String) : ParsedInput =
  let trimmed = Stdlib.String.trim input
  let parts = Stdlib.String.split trimmed " "
  let commandName = Stdlib.List.head parts |> Stdlib.Option.withDefault ""
  let args = Stdlib.List.tail parts |> Stdlib.Option.withDefault []
  let isCompletingCommand =
    Stdlib.List.length parts <= 1L && not (Stdlib.String.endsWith input " ")
  ParsedInput
    { commandName = commandName
      args = args
      isCompletingCommand = isCompletingCommand
      rawInput = input }`,
        calls: [
          {
            name: "Stdlib.String.trim",
            signature: "(s: String) -> String",
            docstring: "Removes leading and trailing whitespace",
            definition: `/// Removes whitespace from both ends of a string
/// Whitespace includes spaces, tabs, and newlines
let trim (s: String) : String =
  s
  |> Stdlib.String.trimStart
  |> Stdlib.String.trimEnd`,
            calls: [],
          },
          {
            name: "Stdlib.String.split",
            signature: "(s: String, separator: String) -> List<String>",
            docstring: "Splits a string by the given separator",
            definition: `/// Splits a string into a list of substrings using the separator
/// Returns a single-element list if separator not found
let split (s: String) (separator: String) : List<String> =
  if Stdlib.String.isEmpty separator then
    Stdlib.String.toList s |> Stdlib.List.map Stdlib.String.fromChar
  else
    Builtin.stringSplit s separator`,
            calls: [],
          },
          {
            name: "Stdlib.List.head",
            signature: "(list: List<'a>) -> Option<'a>",
            docstring: "Returns the first element of the list, or None if empty",
            definition: `/// Returns Some with the first element, or None for empty list
let head (list: List<'a>) : Option<'a> =
  match list with
  | [] -> Stdlib.Option.None
  | first :: _ -> Stdlib.Option.Some first`,
            calls: [],
          },
        ],
      },
      {
        name: "Darklang.Cli.Completion.getPartialCompletion",
        signature: "(parsed: ParsedInput) -> String",
        docstring: "Extracts the partial text being completed from parsed input",
        definition: `let getPartialCompletion (parsed: ParsedInput) : String =
  if parsed.isCompletingCommand then
    parsed.commandName
  else
    Stdlib.List.last parsed.args |> Stdlib.Option.withDefault ""`,
        calls: [
          {
            name: "Stdlib.List.last",
            signature: "(list: List<'a>) -> Option<'a>",
            docstring: "Returns the last element of the list, or None if empty",
            definition: `/// Returns Some with the last element, or None for empty list
let last (list: List<'a>) : Option<'a> =
  match list with
  | [] -> Stdlib.Option.None
  | [x] -> Stdlib.Option.Some x
  | _ :: tail -> Stdlib.List.last tail`,
            calls: [],
          },
        ],
      },
      {
        name: "Darklang.Cli.Core.allCommands",
        signature: "() -> List<Command>",
        docstring: "Returns all registered CLI commands",
        definition: `let allCommands () : List<Command> =
  [ helpCommand
    versionCommand
    runCommand
    buildCommand
    testCommand
    formatCommand
    lspCommand
    replCommand ]`,
        calls: [],
      },
      {
        name: "Stdlib.List.map",
        signature: "(list: List<'a>, fn: ('a) -> 'b) -> List<'b>",
        docstring: "Transforms each element using the given function",
        definition: `/// Applies fn to each element and returns the list of results
/// The resulting list has the same length as the input
let map (list: List<'a>) (fn: 'a -> 'b) : List<'b> =
  Stdlib.List.fold list [] (fun acc item ->
    Stdlib.List.append acc [fn item])`,
        calls: [],
      },
      {
        name: "Stdlib.List.fold",
        signature: "(list: List<'a>, init: 'b, fn: ('b, 'a) -> 'b) -> 'b",
        docstring: "Reduces a list to a single value by applying fn to each element",
        definition: `/// Iterates through list, threading an accumulator through each call to fn
/// Returns the final accumulator value
let fold (list: List<'a>) (init: 'b) (fn: 'b -> 'a -> 'b) : 'b =
  match list with
  | [] -> init
  | head :: tail ->
    let newAcc = fn init head
    Stdlib.List.fold tail newAcc fn`,
        calls: [],
      },
      {
        name: "Stdlib.List.append",
        signature: "(list1: List<'a>, list2: List<'a>) -> List<'a>",
        docstring: "Concatenates two lists",
        definition: `/// Returns a new list with all elements from list1 followed by list2
let append (list1: List<'a>) (list2: List<'a>) : List<'a> =
  Stdlib.List.fold (Stdlib.List.reverse list1) list2 (fun acc item ->
    item :: acc)`,
        calls: [],
      },
      {
        name: "Stdlib.List.filter",
        signature: "(list: List<'a>, fn: ('a) -> Bool) -> List<'a>",
        docstring: "Returns elements where the predicate returns true",
        definition: `/// Returns a new list containing only elements for which fn returns true
/// Preserves the order of elements from the original list
let filter (list: List<'a>) (fn: 'a -> Bool) : List<'a> =
  Stdlib.List.fold list [] (fun acc item ->
    if fn item then
      Stdlib.List.append acc [item]
    else
      acc)`,
        calls: [],
      },
      {
        name: "Stdlib.String.startsWith",
        signature: "(s: String, prefix: String) -> Bool",
        docstring: "Returns true if the string starts with the prefix",
        definition: `/// Returns true if s begins with prefix
/// An empty prefix matches any string
let startsWith (s: String) (prefix: String) : Bool =
  if Stdlib.String.isEmpty prefix then
    true
  else
    let prefixLen = Stdlib.String.length prefix
    let slice = Stdlib.String.slice s 0L prefixLen
    slice == prefix`,
        calls: [],
      },
      {
        name: "Darklang.Cli.Core.findCommand",
        signature: "(name: String) -> Command",
        docstring: "Finds a command by name or alias",
        definition: `let findCommand (name: String) : Command =
  let commands = allCommands ()
  let found =
    Stdlib.List.findFirst commands (fun cmd ->
      cmd.name == name || Stdlib.List.contains cmd.aliases name)
  match found with
  | Some cmd -> cmd
  | None -> unknownCommand name`,
        calls: [
          {
            name: "Stdlib.List.findFirst",
            signature: "(list: List<'a>, fn: ('a) -> Bool) -> Option<'a>",
            docstring: "Returns the first element matching the predicate",
            definition: `/// Returns Some(element) for the first element where fn returns true
/// Returns None if no element matches the predicate
let findFirst (list: List<'a>) (fn: 'a -> Bool) : Option<'a> =
  match list with
  | [] -> Stdlib.Option.None
  | head :: tail ->
    if fn head then
      Stdlib.Option.Some head
    else
      Stdlib.List.findFirst tail fn`,
            calls: [],
          },
          {
            name: "Stdlib.List.contains",
            signature: "(list: List<'a>, item: 'a) -> Bool",
            docstring: "Returns true if the list contains the item",
            definition: `/// Returns true if any element equals the given item
let contains (list: List<'a>) (item: 'a) : Bool =
  Stdlib.List.findFirst list (fun x -> x == item)
  |> Stdlib.Option.isSome`,
            calls: [],
          },
        ],
      },
      {
        name: "handler.complete (dynamic)",
        signature: "(state: AppState, args: List<String>) -> List<String>",
        docstring: "Dynamic call to the command's completion function. Each command defines its own 'complete' function in the Command record.",
        definition: `// This is a dynamic function call - the actual function depends on which
// command was matched by findCommand. Each Command has a 'complete' field:

type Command = {
  name: String
  description: String
  aliases: List<String>
  run: (AppState, List<String>) -> Result<Unit, String>
  complete: (AppState, List<String>) -> List<String>  // <-- this field
}

// Example implementations:

// runCommand.complete - completes file paths
let runComplete (state: AppState) (args: List<String>) : List<String> =
  let partial = Stdlib.List.last args |> Stdlib.Option.withDefault ""
  Stdlib.File.listDirectory state.workingDir
  |> Stdlib.List.filter (fun f -> Stdlib.String.endsWith f ".dark")
  |> Stdlib.List.filter (fun f -> Stdlib.String.startsWith f partial)

// helpCommand.complete - completes command names
let helpComplete (state: AppState) (args: List<String>) : List<String> =
  let commands = allCommands ()
  Stdlib.List.map commands (fun cmd -> cmd.name)`,
        calls: [],
      },
    ],
  },
  "Darklang.HttpMiddleware.handleRequest": {
    name: "Darklang.HttpMiddleware.handleRequest",
    signature: "(request: Stdlib.Http.Request, handlers: List<HttpHandler>) -> Stdlib.Http.Response",
    docstring: "Routes an incoming HTTP request to the appropriate handler and returns the response",
    definition: `let handleRequest
  (request: Stdlib.Http.Request)
  (handlers: List<HttpHandler>)
  : Stdlib.Http.Response =
  match findMatchingHandler request.path handlers with
  | Some handler ->
    let enrichedRequest = applyMiddleware request
    handler.fn enrichedRequest
  | None ->
    Stdlib.Http.Response.notFound "Route not found"`,
    calls: [
      {
        name: "Darklang.HttpMiddleware.findMatchingHandler",
        signature: "(path: String, handlers: List<HttpHandler>) -> Option<HttpHandler>",
        docstring: "Finds the first handler whose route pattern matches the request path",
        definition: `let findMatchingHandler
  (path: String)
  (handlers: List<HttpHandler>)
  : Option<HttpHandler> =
  let pathSegments = Stdlib.String.split path "/"
  handlers
  |> Stdlib.List.findFirst (fun handler ->
    matchRoute handler.pattern pathSegments)`,
        calls: [
          {
            name: "Stdlib.String.split",
            signature: "(s: String, separator: String) -> List<String>",
            docstring: "Splits a string by the given separator",
            definition: `/// Splits a string into a list of substrings using the separator
/// Returns a single-element list if separator not found
/// Returns empty strings for consecutive separators
let split (s: String) (separator: String) : List<String> =
  if Stdlib.String.isEmpty separator then
    Stdlib.String.toList s |> Stdlib.List.map Stdlib.String.fromChar
  else
    Builtin.stringSplit s separator`,
            calls: [],
          },
          {
            name: "Stdlib.List.findFirst",
            signature: "(list: List<'a>, fn: ('a) -> Bool) -> Option<'a>",
            docstring: "Returns the first element matching the predicate, or None",
            definition: `/// Returns Some(element) for the first element where fn returns true
/// Returns None if no element matches the predicate
/// Short-circuits on first match for efficiency
let findFirst (list: List<'a>) (fn: 'a -> Bool) : Option<'a> =
  match list with
  | [] -> Stdlib.Option.None
  | head :: tail ->
    if fn head then
      Stdlib.Option.Some head
    else
      Stdlib.List.findFirst tail fn`,
            calls: [],
          },
        ],
      },
      {
        name: "Darklang.HttpMiddleware.applyMiddleware",
        signature: "(request: Stdlib.Http.Request) -> Stdlib.Http.Request",
        docstring: "Enriches the request with middleware processing (auth, logging, etc.)",
        definition: `let applyMiddleware
  (request: Stdlib.Http.Request)
  : Stdlib.Http.Request =
  request
  |> addRequestId
  |> parseAuthHeader
  |> logIncomingRequest`,
        calls: [
          {
            name: "Darklang.HttpMiddleware.addRequestId",
            signature: "(request: Stdlib.Http.Request) -> Stdlib.Http.Request",
            docstring: "Adds a unique request ID header for tracing",
            definition: `/// Adds a unique X-Request-Id header for distributed tracing
/// Uses UUID v4 for globally unique identifiers
let addRequestId (request: Stdlib.Http.Request) : Stdlib.Http.Request =
  let requestId = Stdlib.Uuid.generate ()
  let requestIdStr = Stdlib.Uuid.toString requestId
  let newHeaders = Stdlib.Dict.set request.headers "X-Request-Id" requestIdStr
  { request with headers = newHeaders }`,
            calls: [],
          },
          {
            name: "Darklang.HttpMiddleware.parseAuthHeader",
            signature: "(request: Stdlib.Http.Request) -> Stdlib.Http.Request",
            docstring: "Parses and validates the Authorization header if present",
            definition: `/// Parses the Authorization header and extracts auth info
/// Supports Bearer tokens and Basic auth schemes
/// Sets request.auth to the parsed credentials if valid
let parseAuthHeader (request: Stdlib.Http.Request) : Stdlib.Http.Request =
  match Stdlib.Dict.get request.headers "Authorization" with
  | Some authValue ->
    let authInfo = parseAuthValue authValue
    { request with auth = authInfo }
  | None ->
    { request with auth = AuthInfo.Anonymous }`,
            calls: [],
          },
        ],
      },
      {
        name: "Stdlib.Http.Response.notFound",
        signature: "(message: String) -> Stdlib.Http.Response",
        docstring: "Creates a 404 Not Found response with the given message",
        definition: `/// Creates a 404 Not Found HTTP response
/// Sets appropriate Content-Type header for text response
let notFound (body: String) : Response =
  let headers =
    Stdlib.Dict.empty
    |> Stdlib.Dict.set "Content-Type" "text/plain; charset=utf-8"
  Response
    { statusCode = 404L
      headers = headers
      body = Stdlib.String.toBytes body }`,
        calls: [],
      },
    ],
  },
};

const AVAILABLE_FUNCTIONS = Object.keys(FAKE_CALL_GRAPH);

export class CallGraphPanel {
  public static currentPanel: CallGraphPanel | undefined;
  public static readonly viewType = "darklang.callGraph";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    if (CallGraphPanel.currentPanel) {
      CallGraphPanel.currentPanel._panel.reveal(column);
      return;
    }

    const panel = vscode.window.createWebviewPanel(
      CallGraphPanel.viewType,
      "Call Graph Explorer",
      column || vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [extensionUri],
      },
    );

    CallGraphPanel.currentPanel = new CallGraphPanel(panel, extensionUri);
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    CallGraphPanel.currentPanel = new CallGraphPanel(panel, extensionUri);
  }

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    this._panel = panel;
    this._extensionUri = extensionUri;

    this._update();

    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    this._panel.webview.onDidReceiveMessage(
      async message => {
        switch (message.command) {
          case "loadGraph":
            this._loadGraph(message.fnName);
            return;
        }
      },
      null,
      this._disposables,
    );
  }

  private _loadGraph(fnName: string) {
    const graph = FAKE_CALL_GRAPH[fnName];
    this._panel.webview.postMessage({
      command: "graphLoaded",
      graph: graph || null,
      error: graph ? null : `Function "${fnName}" not found`,
    });
  }

  public dispose() {
    CallGraphPanel.currentPanel = undefined;
    this._panel.dispose();
    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
  }

  private _update() {
    this._panel.webview.html = this._getHtmlForWebview();
  }

  private _getHtmlForWebview() {
    const functionsJson = JSON.stringify(AVAILABLE_FUNCTIONS);

    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Call Graph Explorer</title>
    <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }

        body {
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 24px;
            overflow-x: auto;
        }

        .container { min-width: 800px; }

        .header {
            display: flex;
            align-items: center;
            gap: 12px;
            margin-bottom: 24px;
        }

        .header h1 { font-size: 20px; font-weight: 500; }

        .header-icon { width: 24px; height: 24px; color: #8b5cf6; }

        .search-section {
            background: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 8px;
            padding: 16px;
            margin-bottom: 24px;
        }

        .search-row {
            display: flex;
            gap: 12px;
            align-items: center;
        }

        label {
            font-size: 12px;
            font-weight: 500;
            color: var(--vscode-descriptionForeground);
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        select, input {
            flex: 1;
            background: var(--vscode-input-background);
            border: 1px solid var(--vscode-input-border);
            color: var(--vscode-input-foreground);
            border-radius: 4px;
            padding: 10px 12px;
            font-family: var(--vscode-editor-font-family);
            font-size: 14px;
        }

        select:focus, input:focus {
            outline: none;
            border-color: var(--vscode-focusBorder);
        }

        .btn {
            padding: 10px 20px;
            border-radius: 4px;
            border: none;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            transition: all 0.15s;
            background: #8b5cf6;
            color: white;
        }

        .btn:hover { background: #7c3aed; }

        .main-layout {
            display: flex;
            gap: 24px;
        }

        .graph-container {
            flex: 1;
            background: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 8px;
            padding: 24px;
            overflow-x: auto;
            min-width: 400px;
        }

        .definition-panel {
            width: 450px;
            background: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 8px;
            padding: 16px;
            display: none;
        }

        .definition-panel.visible { display: block; }

        .definition-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 12px;
            padding-bottom: 12px;
            border-bottom: 1px solid var(--vscode-panel-border);
        }

        .definition-title {
            font-size: 14px;
            font-weight: 600;
            color: #8b5cf6;
            font-family: var(--vscode-editor-font-family);
        }

        .definition-close {
            background: none;
            border: none;
            color: var(--vscode-descriptionForeground);
            cursor: pointer;
            font-size: 18px;
            padding: 4px;
        }

        .definition-close:hover { color: var(--vscode-foreground); }

        .definition-signature {
            font-family: var(--vscode-editor-font-family);
            font-size: 12px;
            color: #22c55e;
            padding: 8px 10px;
            background: rgba(34, 197, 94, 0.1);
            border-radius: 4px;
            margin-bottom: 12px;
        }

        .definition-docstring {
            font-size: 12px;
            color: var(--vscode-descriptionForeground);
            margin-bottom: 12px;
            line-height: 1.5;
        }

        .definition-code {
            font-family: var(--vscode-editor-font-family);
            font-size: 12px;
            line-height: 1.6;
            background: var(--vscode-editor-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 4px;
            padding: 12px;
            white-space: pre;
            overflow-x: auto;
            color: var(--vscode-foreground);
        }

        .empty-state {
            text-align: center;
            padding: 48px;
            color: var(--vscode-descriptionForeground);
        }

        .empty-state svg {
            width: 48px;
            height: 48px;
            margin-bottom: 16px;
            opacity: 0.5;
        }

        /* Vertical tree for main chain */
        .tree-vertical {
            display: flex;
            flex-direction: column;
            gap: 0;
        }

        .main-node {
            position: relative;
            padding-left: 0;
        }

        .main-node-content {
            background: var(--vscode-editor-background);
            border: 2px solid #8b5cf6;
            border-radius: 8px;
            padding: 12px 16px;
            cursor: pointer;
            transition: all 0.15s;
            display: inline-block;
        }

        .main-node-content:hover {
            background: rgba(139, 92, 246, 0.15);
            box-shadow: 0 2px 8px rgba(139, 92, 246, 0.3);
        }

        .main-node-content.selected {
            background: rgba(139, 92, 246, 0.2);
            box-shadow: 0 0 0 2px rgba(139, 92, 246, 0.4);
        }

        .main-node-name {
            font-family: var(--vscode-editor-font-family);
            font-size: 13px;
            font-weight: 600;
            color: #8b5cf6;
        }

        .main-node-signature {
            font-family: var(--vscode-editor-font-family);
            font-size: 11px;
            color: var(--vscode-descriptionForeground);
            margin-top: 4px;
        }

        /* Connector line between main nodes */
        .vertical-connector {
            width: 2px;
            height: 24px;
            background: linear-gradient(to bottom, #8b5cf6, rgba(139, 92, 246, 0.3));
            margin-left: 24px;
        }

        /* Horizontal children (nested calls) */
        .nested-calls {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
            margin: 12px 0 0 40px;
            padding-left: 16px;
            border-left: 2px dashed rgba(139, 92, 246, 0.3);
        }

        .nested-node {
            background: var(--vscode-editor-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 4px;
            padding: 6px 10px;
            cursor: pointer;
            transition: all 0.15s;
            font-size: 12px;
        }

        .nested-node:hover {
            border-color: #8b5cf6;
            background: rgba(139, 92, 246, 0.1);
        }

        .nested-node.selected {
            border-color: #8b5cf6;
            background: rgba(139, 92, 246, 0.15);
        }

        .nested-node-name {
            font-family: var(--vscode-editor-font-family);
            color: #a78bfa;
            font-weight: 500;
        }

        .error-message {
            color: #ef4444;
            padding: 16px;
            text-align: center;
        }

        .hint {
            font-size: 12px;
            color: var(--vscode-descriptionForeground);
            margin-top: 8px;
        }

        .click-hint {
            font-size: 11px;
            color: var(--vscode-descriptionForeground);
            font-style: italic;
            margin-bottom: 16px;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <svg class="header-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                <circle cx="12" cy="12" r="3"/>
                <path d="M12 2v4m0 12v4M2 12h4m12 0h4"/>
                <path d="M4.93 4.93l2.83 2.83m8.48 8.48l2.83 2.83M4.93 19.07l2.83-2.83m8.48-8.48l2.83-2.83"/>
            </svg>
            <h1>Call Graph Explorer</h1>
        </div>

        <div class="search-section">
            <div class="search-row">
                <label>Entry Point</label>
                <select id="fnSelect">
                    <option value="">Select a function...</option>
                </select>
                <button class="btn" id="loadBtn">Explore</button>
            </div>
            <div class="hint">Select a function to see what it calls and what those functions call.</div>
        </div>

        <div class="main-layout">
            <div class="graph-container" id="graphContainer">
                <div class="empty-state">
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5">
                        <circle cx="12" cy="12" r="3"/>
                        <path d="M12 2v4m0 12v4M2 12h4m12 0h4"/>
                    </svg>
                    <div>Select a function to explore its call graph</div>
                </div>
            </div>

            <div class="definition-panel" id="definitionPanel">
                <div class="definition-header">
                    <span class="definition-title" id="defTitle">Function Definition</span>
                    <button class="definition-close" id="defClose">&times;</button>
                </div>
                <div class="definition-signature" id="defSignature"></div>
                <div class="definition-docstring" id="defDocstring"></div>
                <div class="definition-code" id="defCode"></div>
            </div>
        </div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();
        const functions = ${functionsJson};
        let currentGraph = null;
        let selectedNodeId = null;

        // Populate dropdown
        const select = document.getElementById('fnSelect');
        functions.forEach(fn => {
            const option = document.createElement('option');
            option.value = fn;
            option.textContent = fn;
            select.appendChild(option);
        });

        document.getElementById('loadBtn').addEventListener('click', () => {
            const fnName = select.value;
            if (fnName) {
                vscode.postMessage({ command: 'loadGraph', fnName });
            }
        });

        select.addEventListener('change', () => {
            const fnName = select.value;
            if (fnName) {
                vscode.postMessage({ command: 'loadGraph', fnName });
            }
        });

        document.getElementById('defClose').addEventListener('click', () => {
            document.getElementById('definitionPanel').classList.remove('visible');
            clearSelection();
        });

        function clearSelection() {
            document.querySelectorAll('.selected').forEach(el => el.classList.remove('selected'));
            selectedNodeId = null;
        }

        function showDefinition(node, nodeId) {
            clearSelection();
            selectedNodeId = nodeId;
            const nodeEl = document.querySelector('[data-node-id="' + nodeId + '"]');
            if (nodeEl) nodeEl.classList.add('selected');

            const panel = document.getElementById('definitionPanel');
            document.getElementById('defTitle').textContent = node.name;
            document.getElementById('defSignature').textContent = node.signature || 'No signature available';
            document.getElementById('defDocstring').textContent = node.docstring || 'No documentation available';
            document.getElementById('defCode').textContent = node.definition || '// Definition not available';
            panel.classList.add('visible');
        }

        function getShortName(fullName) {
            const parts = fullName.split('.');
            return parts.length > 1 ? parts.slice(-1)[0] : fullName;
        }

        // Store nodes by ID for click handling
        let nodeDataMap = {};

        function renderGraph(graph) {
            currentGraph = graph;
            nodeDataMap = {};
            let nodeCounter = 0;

            function renderMainNode(node, isFirst = false) {
                const nodeId = 'node-' + (nodeCounter++);
                nodeDataMap[nodeId] = node;
                const hasNestedCalls = node.calls && node.calls.length > 0;
                const shortName = getShortName(node.name);

                let html = '';

                // Add vertical connector before (except first)
                if (!isFirst) {
                    html += '<div class="vertical-connector"></div>';
                }

                html += '<div class="main-node">';
                html += '<div class="main-node-content" data-node-id="' + nodeId + '">';
                html += '<div class="main-node-name">' + escapeHtml(shortName) + '</div>';
                if (node.signature) {
                    html += '<div class="main-node-signature">' + escapeHtml(node.signature) + '</div>';
                }
                html += '</div>';

                // Render nested calls horizontally
                if (hasNestedCalls) {
                    html += '<div class="nested-calls">';
                    node.calls.forEach(child => {
                        const childNodeId = 'node-' + (nodeCounter++);
                        nodeDataMap[childNodeId] = child;
                        const childShortName = getShortName(child.name);
                        html += '<div class="nested-node" data-node-id="' + childNodeId + '">';
                        html += '<span class="nested-node-name">' + escapeHtml(childShortName) + '</span>';
                        html += '</div>';
                    });
                    html += '</div>';
                }

                html += '</div>';
                return html;
            }

            let html = '<div class="click-hint">Click any function to see its definition</div>';
            html += '<div class="tree-vertical">';

            // Root node
            html += renderMainNode(graph, true);

            // First-level children as main vertical nodes
            if (graph.calls && graph.calls.length > 0) {
                graph.calls.forEach(child => {
                    html += renderMainNode(child, false);
                });
            }

            html += '</div>';
            return html;
        }

        function escapeHtml(text) {
            const div = document.createElement('div');
            div.textContent = text;
            return div.innerHTML;
        }

        window.addEventListener('message', event => {
            const message = event.data;
            const container = document.getElementById('graphContainer');

            switch (message.command) {
                case 'graphLoaded':
                    document.getElementById('definitionPanel').classList.remove('visible');
                    if (message.error) {
                        container.innerHTML = '<div class="error-message">' + escapeHtml(message.error) + '</div>';
                    } else if (message.graph) {
                        container.innerHTML = renderGraph(message.graph);
                        attachClickHandlers();
                    }
                    break;
            }
        });

        function attachClickHandlers() {
            document.querySelectorAll('[data-node-id]').forEach(el => {
                el.addEventListener('click', () => {
                    const nodeId = el.dataset.nodeId;
                    const node = nodeDataMap[nodeId];
                    if (node) {
                        showDefinition(node, nodeId);
                    }
                });
            });
        }
    </script>
</body>
</html>`;
  }
}
