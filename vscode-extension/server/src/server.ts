import * as child_process from "child_process";
import * as util from "util";
const exec = util.promisify(child_process.exec);

import {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  InitializeParams,
  CompletionItem,
  CompletionItemKind,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  InitializeResult,
  TextDocumentChangeEvent,
  SemanticTokensRequest,
  SemanticTokens,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

// docs: https://github.com/tree-sitter/tree-sitter/blob/master/lib/binding_web/tree-sitter-web.d.ts
const Parser = require("web-tree-sitter");

import { ComputeDiagnosticsOutput } from "./darkTypes";
import * as DT2LT from "./darkTypesToLspTypes";

// TODO share across server/client, if relevant?
// or generate these from Darklang code?
//
// note: these are referenced by their _index_!
// i.e. 'keyword' is 0, 'string' is 4, etc.
const tokenTypes = [
  "keyword", // for words 'let' and 'in'
  "function", // for function names/identifiers
  "parameter", // for function parameter identifiers
  "type", // for type names like Int, Bool, etc.
  "string", // for string literals
  "operator", // for operators like +, -
  "variable", // for general identifiers
];
const tokenModifiers: string[] = [];

const connection = createConnection(ProposedFeatures.all);

const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

function shellEscape(str: string): string {
  return `'${str.replace(/'/g, "'\\''")}'`;
}

async function runDarkCli(...args: string[]) {
  const darkCliPath = "/home/dark/app/backend/Build/out/Cli/Debug/net7.0/Cli";

  const cmd = `${darkCliPath} ${args.map(shellEscape).join(" ")}`;
  const { stdout, stderr } = await exec(cmd);
  return { stdout, stderr };
}

async function gatherAndReportDiagnostics(
  textDocument: TextDocument,
): Promise<void> {
  const diagnosticsFromDarkResponse = await runDarkCli(
    "@PACKAGE.Darklang.LanguageTools.LanguageServerProtocol.getDiagnostics",
    textDocument.uri,
    JSON.stringify(textDocument.getText()),
  );

  if (diagnosticsFromDarkResponse.stderr) {
    console.error("stderr", diagnosticsFromDarkResponse.stderr);
  } else {
    console.log("got diagnostics back", diagnosticsFromDarkResponse.stdout);
    const diagnosticsFromDark: ComputeDiagnosticsOutput = JSON.parse(
      diagnosticsFromDarkResponse.stdout,
    );
    const diagnostics = diagnosticsFromDark.diagnostics.map(DT2LT.diagnostic);
    connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
  }
}

let parser: any;

connection.onInitialize(async (_params: InitializeParams) => {
  console.log("initializing parser");
  await Parser.init();
  parser = new Parser();
  const DarklangParser = await Parser.Language.load(
    "/home/dark/app/vscode-extension/static/tree-sitter/tree-sitter-darklang.wasm",
  );
  parser.setLanguage(DarklangParser);
  console.log("parser initialized", DarklangParser);

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      completionProvider: { resolveProvider: true },
    },
  };

  result.capabilities.semanticTokensProvider = {
    legend: {
      tokenTypes: tokenTypes,
      tokenModifiers: tokenModifiers,
    },
    range: false,
    full: {
      delta: false,
    },
  };

  //console.log("initialization result: ", JSON.stringify(result));
  return result;
});

connection.onDidChangeConfiguration(change => {
  console.log("onDidChangeConfiguration");

  documents.all().forEach(gatherAndReportDiagnostics);
});

// when a document is changed or saved, we want to re-run diagnostics
let changeToProcessNext: null | TextDocumentChangeEvent<TextDocument> = null;
let processing = false;
const processChange = async () => {
  if (processing || !changeToProcessNext) return;
  processing = true;
  try {
    const doc = changeToProcessNext.document;
    changeToProcessNext = null;
    await gatherAndReportDiagnostics(doc);
  } finally {
    processing = false;
    if (changeToProcessNext) {
      processChange();
    }
  }
};
documents.onDidSave(change => {
  changeToProcessNext = change;
  processChange();
});
documents.onDidChangeContent(change => {
  changeToProcessNext = change;
  processChange();
});

// (how) should we autocomplete?
// (currently this just provides 2 bogus autocompletes)
connection.onCompletion(
  (_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
    return [
      { label: "TypeScript", kind: CompletionItemKind.Text, data: 1 },
      { label: "JavaScript", kind: CompletionItemKind.Text, data: 2 },
    ];
  },
);

// once you hover over some completion, this fills in some details (I think)
connection.onCompletionResolve((item: CompletionItem): CompletionItem => {
  if (item.data === 1) {
    item.detail = "TypeScript details";
    item.documentation = "TypeScript documentation";
  } else if (item.data === 2) {
    item.detail = "JavaScript details";
    item.documentation = "JavaScript documentation";
  }
  return item;
});

documents.listen(connection);

connection.listen();

// following are attempts at semantic-tree-ifying our parsed code, to be highlighted by VS Code

// v1 - this is ChatGPT-generated, largely, and is kinda broken but proved the point
// connection.onRequest(
//   SemanticTokensRequest.type,
//   ({ textDocument }): SemanticTokens => {
//     const tokens: number[] = [];
//     let lastLine = 0;
//     let lastStartChar = 0;

//     const encodeToken = (
//       line: number,
//       startChar: number,
//       length: number,
//       tokenType: number,
//     ) => {
//       const deltaLine = line - lastLine;
//       const deltaStart =
//         deltaLine === 0 ? startChar - lastStartChar : startChar;

//       tokens.push(deltaLine, deltaStart, length, tokenType, 0); // Assuming no tokenModifiers for simplicity

//       lastLine = line;
//       lastStartChar = startChar + length;
//     };

//     console.log("Processing semantic tokens request");

//     const content = documents.get(textDocument.uri)?.getText() || "";
//     const tree = parser.parse(content);

//     const processNode = (node: any) => {
//       //console.log("processing node", node);
//       const { type, startPosition, endPosition } = node;
//       const line = startPosition.row;
//       const startChar = startPosition.column;
//       const length = endPosition.column - startPosition.column;

//       switch (type) {
//         case "let":
//           encodeToken(line, startChar, length, 0);
//           break;

//         case "identifier":
//           if (
//             node.parent &&
//             node.parent.type === "fn_def" &&
//             node.parent.name === node
//           ) {
//             encodeToken(line, startChar, length, 1);
//           } else {
//             encodeToken(line, startChar, length, 6);
//           }
//           break;

//         case "fn_param_def":
//           encodeToken(line, startChar, length, 2);
//           break;

//         case "Int":
//         case "Bool":
//         case "Float":
//         case "String":
//         case "Char":
//           encodeToken(line, startChar, length, 3);
//           break;

//         case "string_literal":
//           encodeToken(line, startChar, length, 4);
//           break;

//         case "+":
//         case "-":
//           encodeToken(line, startChar, length, 5);
//           break;

//         default:
//           break;
//       }

//       // Recurse through child nodes
//       for (const child of node.children) {
//         processNode(child);
//       }
//     };

//     processNode(tree.rootNode);

//     console.log("Encoded tokens:", tokens);
//     return {
//       data: tokens,
//     };
//   },
// );

// v2: _manually_ returning the tokens that we want, for the exact code of:
//```
//let add (a: Int) (b: Int): Int =
//  let sum = a + b
//  sum
//```
connection.onRequest(
  SemanticTokensRequest.type,
  ({ textDocument }): SemanticTokens => {
    // our token types
    // | 0 | keyword   | words 'let' and 'in'            |
    // | 1 | function  | function names/identifiers      |
    // | 2 | parameter | function parameter identifiers  |
    // | 3 | type      | type names like Int, Bool, etc. |
    // | 4 | string    | string literals                 |
    // | 5 | operator  | operators like +, -             |
    // | 6 | variable  | general identifiers             |

    // code sample:
    // ```fsharp
    // let add (a: Int) (b: Int): Int =
    //   let sum = a + b
    //   sum
    // ```

    // | thing | ΔLine | ΔStart | length | type | modifier |
    // |-------|-------|--------|--------|------|----------|
    // | let   | 0     |      0 |      3 |    0 |        0 |
    // | add   | 0     |      1 |      3 |    6 |        0 |
    // | a     | ...   |        |        |      |          |
    // | Int   |       |        |        |      |          |
    // | b     |       |        |        |      |          |
    // | Int   |       |        |        |      |          |
    // | Int   |       |        |        |      |          |
    // | let   |       |        |        |      |          |
    // | sum   |       |        |        |      |          |
    // | a     |       |        |        |      |          |
    // | b     |       |        |        |      |          |
    // | sum   |       |        |        |      |          |
    // |       |       |        |        |      |          |

    // deltaLine: The difference in lines from the previous token.
    // deltaStart: The difference in characters (columns) from the previous token on the same line (or from the start of the line if it's the first token on that line).
    // length: The length of the token.
    // tokenType: The index in the token types array declared in the legend of the semantic token provider.
    // tokenModifiers: A bitset representing the token modifiers. Each bit of the value represents an index in the token modifiers array declared in the legend of the semantic token provider.

    // prettier-ignore
    const tokens =
      [ 0, 0, 3, 0, 0, // let
        0, 1, 3, 6, 0. // add
        // TODO: continue
      ]

    return {
      data: tokens,
    };
  },
);

// vFuture TODO: in a future pass, use this tree to replace all the below syntax-highlighting logic
// our parser includes a lot of things that _shouldn't_ be tokenized
// (like `fn_def`s which are really just wrappers around other tokens)
// , and the other tokens should use context such as "in a `fn_def`" to determine their type

// TODO: typescriptify this
// function simplifyTree(cursor: any): any {
//   let children = [];

//   if (cursor.gotoFirstChild()) {
//     do {
//       children.push(simplifyTree(cursor));
//     } while (cursor.gotoNextSibling());

//     cursor.gotoParent();
//   }

//   return {
//     typ: cursor.nodeType,
//     text: cursor.nodeText,
//     fieldName: cursor.currentFieldName(),
//     children: children,
//     startPosition: cursor.startPosition,
//     endPosition: cursor.endPosition,
//   };
// }
//
// connection.onRequest(
//   SemanticTokensRequest.type,
//   ({ textDocument }): SemanticTokens => {
//     console.log("Processing semantic tokens request");
//
//     const content = documents.get(textDocument.uri)?.getText() || "";
//     const tree = parser.parse(content);
//
//     let simpleTree = simplifyTree(tree.rootNode.walk());
//     console.log("simpletree", simpleTree);
//
//     // TODO: continue
//
//     return { data: [] };
//   },
// );
