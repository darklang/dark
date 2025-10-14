import { ParsedUrl } from "../urlPatternRouter";

/**
 * Content provider for package browsing URLs
 * Handles: dark://package/Name.Space.item[?view=type]
 */
export class PackageContentProvider {
  static getContent(parsedUrl: ParsedUrl): string {
    const { target, view } = parsedUrl;

    if (!target) {
      return this.getPackageListContent();
    }

    switch (view) {
      case 'ast':
        return this.getAstView(target);

      default:
        return this.getSourceView(target);
    }
  }

  private static getSourceView(target: string): string {
    // Handle different types of targets
    if (target === 'Darklang.Stdlib.List.map') {
      return this.getListMapContent();
    } else if (target.includes('Darklang.Stdlib')) {
      return this.getStdlibItemContent(target);
    } else {
      return this.getGenericPackageContent(target);
    }
  }

  private static getAstView(target: string): string {
    return `# AST View: ${target}

## Abstract Syntax Tree

\`\`\`
FunctionDef {
  name: "${target.split('.').pop()}"
  parameters: [
    Parameter { name: "fn", type: "'a -> 'b" },
    Parameter { name: "list", type: "List<'a>" }
  ]
  returnType: "List<'b>"
  body: MatchExpression {
    expr: Variable("list")
    cases: [
      Case {
        pattern: EmptyList
        body: EmptyList
      },
      Case {
        pattern: Cons(Variable("head"), Variable("tail"))
        body: Cons(
          FunctionCall(Variable("fn"), [Variable("head")]),
          FunctionCall(Variable("map"), [Variable("fn"), Variable("tail")])
        )
      }
    ]
  }
}
\`\`\`

## Type Inference

- **Input Types**: \`'a -> 'b\`, \`List<'a>\`
- **Output Type**: \`List<'b>\`
- **Type Variables**: \`'a\`, \`'b\` (polymorphic)
- **Constraints**: None

## Compilation Target

\`\`\`javascript
function map(fn, list) {
  if (list.length === 0) return [];
  return [fn(list[0])].concat(map(fn, list.slice(1)));
}
\`\`\``;
  }

  private static getListMapContent(): string {
    return `// Darklang.Stdlib.List.map - Maps a function over a list

let map (fn: 'a -> 'b) (list: List<'a>): List<'b> =
  match list with
  | [] -> []
  | head :: tail -> (fn head) :: (map fn tail)

// Examples:
// map (fun x -> x * 2) [1L; 2L; 3L] returns [2L; 4L; 6L]
// map String.length ["hello"; "world"] returns [5L; 5L]

// Tests:
Stdlib.Test.expect (map (fun x -> x * 2L) []) []
Stdlib.Test.expect (map (fun x -> x * 2L) [1L; 2L; 3L]) [2L; 4L; 6L]
Stdlib.Test.expect (map String.length ["hi"]) [2L]`;
  }

  private static getStdlibItemContent(target: string): string {
    const itemName = target.split('.').pop();

    // Return realistic source code based on the function
    if (target === 'Darklang.Stdlib.List.head') {
      return `module Darklang.Stdlib.List

/// Returns {{Some}} the head (first value) of a list.
/// Returns {{None}} if the list is empty.
let head (list: List<'a>) : Option.Option<'a> =
  match list with
  | [] -> Option.Option.None
  | head :: _ -> Option.Option.Some head`;
    } else if (target === 'Darklang.Stdlib.List.map') {
      return `module Darklang.Stdlib.List

/// Returns a new list with <param fn> applied to each element
let map (fn: 'a -> 'b) (list: List<'a>) : List<'b> =
  match list with
  | [] -> []
  | head :: tail ->
    let mapped = fn head
    let rest = map fn tail
    mapped :: rest`;
    } else if (target === 'Darklang.Stdlib.List.filter') {
      return `module Darklang.Stdlib.List

/// Returns elements where <param fn> returns true
let filter (fn: 'a -> Bool) (list: List<'a>) : List<'a> =
  match list with
  | [] -> []
  | head :: tail ->
    let rest = filter fn tail
    if fn head then
      head :: rest
    else
      rest`;
    } else if (target === 'Darklang.Stdlib.List.reverse') {
      return `module Darklang.Stdlib.List

let reverseHelper (list: List<'a>) (acc: List<'a>) : List<'a> =
  match list with
  | [] -> acc
  | head :: tail -> reverseHelper tail (head :: acc)

/// Returns a reversed copy of <param list>
let reverse (list: List<'a>) : List<'a> =
  reverseHelper list []`;
    } else if (target === 'Darklang.Stdlib.String.isEmpty') {
      return `module Darklang.Stdlib.String

/// Returns {{true}} if <param str> is the empty string {{""}}
let isEmpty (str: String) : Bool =
  Builtin.stringLength str == 0L`;
    } else if (target === 'Darklang.Stdlib.String.length') {
      return `module Darklang.Stdlib.String

/// Returns the length of <param str>
let length (str: String) : Int64 =
  Builtin.stringLength str`;
    } else if (target === 'Darklang.LanguageTools.Parser.parseExpression') {
      return `module Darklang.LanguageTools.Parser

/// Parses a Darklang expression from source text
let parseExpression (source: String) : Result<Expr, ParseError> =
  let tokens = Lexer.tokenize source

  match tokens with
  | Error lexErr -> Error (ParseError.LexError lexErr)
  | Ok tokenList ->
    let parser = Parser.create tokenList
    Parser.parseExpr parser`;
    } else if (target === 'Stachu.Json.serialize') {
      return `module Stachu.Json

/// Serializes a value to JSON string
let serialize (value: 'a) : String =
  match value with
  | () -> "null"
  | true -> "true"
  | false -> "false"
  | n when typeof<Int64> -> Stdlib.Int64.toString n
  | s when typeof<String> -> "\\"" ++ s ++ "\\""
  | _ -> Builtin.jsonSerialize value`;
    } else if (target === 'Stachu.Json.deserialize') {
      return `module Stachu.Json

/// Deserializes JSON string to a value
let deserialize (json: String) : Result<'a, JsonError> =
  Builtin.jsonDeserialize json`;
    } else if (target === 'Stachu.Timespan.fromSeconds') {
      return `module Stachu.Timespan

type Timespan = {
  totalSeconds: Int64
  totalMilliseconds: Int64
}

/// Creates a Timespan from seconds
let fromSeconds (seconds: Int64) : Timespan =
  {
    totalSeconds = seconds
    totalMilliseconds = seconds * 1000L
  }`;
    } else {
      return `module ${target.split('.').slice(0, -1).join('.')}

// ${target}
// Standard library function

let ${itemName} = (* Implementation would be shown here *)`;
    }
  }

  private static getGenericPackageContent(target: string): string {
    return `# ${target}
(imagine we have package content here)`;
  }


  private static getPackageListContent(): string {
    return `# Darklang Packages
    TODO: some pretty page`;
  }
}