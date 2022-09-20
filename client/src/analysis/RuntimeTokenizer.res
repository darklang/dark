open Prelude
module Expr = RuntimeTypes.Expr

/// This is a copy of the FluidTokenizer, adapted for RuntimeTypes. It just uses
/// strings instead of tokens, and has some features removed (eg multiline strings)

module Builder = {
  type t = {
    @ocaml.doc(" [tokens] list is kept reversed while being built up, as adding
            * things to the front of the list is an order of magnitude faster.
            * We were having large slowdowns on large handlers before this. ")
    tokens: list<string>,
    @ocaml.doc(" [indent] tracks the indent after a newline ")
    indent: int,
    @ocaml.doc(" [xPos] tracks the indent for nesting.
            * `None` indicates it's ready to go after a newline ")
    xPos: option<int>,
  }

  let rec endsInNewline = (b: t): bool =>
    // The latest token is on the front
    switch b.tokens {
    | list{"\n", ..._} => true
    | list{"  ", ...tail} => endsInNewline({...b, tokens: tail})
    | _ => false
    }

  let empty = {
    tokens: list{},
    xPos: Some(0),
    indent: 0,
  }

  let lineLimit = 120

  let listLimit = 60

  // # of chars in a tuple before we should wrap
  let tupleLimit = 60

  let makeSpaces = len => String.repeat(~count=len, " ")

  let add = (token: string, b: t): t => {
    let tokenLength = token |> String.length
    let (newTokens, xPos) = // Add new tokens on the front
    if endsInNewline(b) {
      (
        if b.indent != 0 {
          list{token, makeSpaces(b.indent), ...b.tokens}
        } else {
          list{token, ...b.tokens}
        },
        Some(b.indent + tokenLength),
      )
    } else {
      let newXPos = switch token {
      | "\n" => None
      | _ =>
        let old = Option.unwrap(b.xPos, ~default=b.indent)
        Some(old + tokenLength)
      }

      (list{token, ...b.tokens}, newXPos)
    }

    {...b, tokens: newTokens, xPos: xPos}
  }

  let addIf = (cond: bool, token: string, b: t): t =>
    if cond {
      add(token, b)
    } else {
      b
    }

  /* Take a list of 'a, and iterate through them, adding them to `b` by
   * calling `f` on them */
  let addIter = (xs: list<'a>, ~f: (int, 'a, t) => t, b: t): t =>
    List.fold(xs, ~initial=(b, 0), ~f=((b, i), x) => (f(i, x, b), i + 1)) |> Tuple2.first

  let addMany = (tokens: list<string>, b: t): t =>
    List.fold(tokens, ~initial=b, ~f=(acc, t) => add(t, acc))

  @ocaml.doc(" [indentBy ~ident ~f b] calls [f] with a modified [b] having additional
    * indentation by [indent] characters (that is, b.indent + ~indent), then
    * returns the result of that [f b] invocation with the original indent of
    * [b] restored. ")
  let indentBy = (~indent: int, ~f: t => t, b: t): t => {
    let oldIndent = b.indent
    {...b, indent: b.indent + indent} |> f |> (b => {...b, indent: oldIndent})
  }

  let addNested = (~f: t => t, b: t): t => {
    let oldIndent = b.indent
    let newIndent = Option.unwrap(~default=b.indent, b.xPos)
    {...b, indent: newIndent} |> f |> (b => {...b, indent: oldIndent})
  }

  let addNewlineIfNeeded = (b: t): t =>
    if endsInNewline(b) {
      b
    } else {
      add("\n", b)
    }

  let asTokens = (b: t): list<string> =>
    // Tokens are stored reversed
    b.tokens->List.reverse

  let toString = (b: t): string =>
    // Tokens are stored reversed
    b.tokens->List.reverse->List.join(~sep="")
}

let rec patternToTokens = (matchID: id, p: RuntimeTypes.MatchPattern.t, ~idx: int): list<
  string,
> => {
  switch p {
  | MPVariable(_, name) => list{name}
  | MPConstructor(_, name, args) =>
    let args = List.map(args, ~f=a => list{" ", ...patternToTokens(matchID, a, ~idx)})

    List.flatten(list{list{name}, ...args})
  | MPInteger(_, i) => list{Int64.to_string(i)}
  | MPBool(_, b) =>
    if b {
      list{"true"}
    } else {
      list{"false"}
    }
  | MPString(_, str) => list{"\"", str, "\""}
  | MPCharacter(_, c) => list{"'", c, "'"}
  | MPFloat(_, f) => list{Float.to_string(f)}

  | MPNull(_) => list{"null"}
  | MPBlank(_) => list{"___"}
  | MPTuple(_, first, second, theRest) =>
    let subPatterns = list{first, second, ...theRest}

    let subPatternCount = List.length(subPatterns)

    let middlePart =
      subPatterns
      |> List.mapWithIndex(~f=(i, p) => {
        let isLastPattern = i == subPatternCount - 1
        let subpatternTokens = patternToTokens(matchID, p, ~idx)

        if isLastPattern {
          subpatternTokens
        } else {
          List.append(subpatternTokens, list{", "})
        }
      })
      |> List.flatten

    List.flatten(list{list{"("}, middlePart, list{")"}})
  }
}

let rec toTokens' = (e: Expr.t, b: Builder.t): Builder.t => {
  open Builder

  let nest = (~indent, e: Expr.t, b: Builder.t): Builder.t => {
    b |> indentBy(~indent, ~f=addNested(~f=toTokens'(e)))
  }

  let addArgs = (args: list<Expr.t>, b: Builder.t): Builder.t => {
    let reflow = {
      let tokens =
        args
        |> List.map(~f=a => toTokens'(a, Builder.empty))
        |> List.map(~f=Builder.asTokens)
        |> List.flatten

      let length =
        (tokens |> List.map(~f=String.length))->List.sum(module(Int))
        |> \"+" /* separators, including at the front */(List.length(args))
        |> \"+"(Option.unwrap(~default=0, b.xPos))

      let tooLong = length > lineLimit
      let needsNewlineBreak =
        // newlines aren't disruptive in the last argument
        args
        |> List.initial
        |> Option.unwrap(~default=list{})
        |> List.map(~f=a => toTokens'(a, Builder.empty))
        |> List.map(~f=Builder.asTokens)
        |> List.flatten
        |> List.any(~f=String.includes(~substring="\n"))

      tooLong || needsNewlineBreak
    }

    b |> addIter(args, ~f=(_, e, b) =>
      if reflow {
        b |> addNewlineIfNeeded |> nest(~indent=2, e)
      } else {
        b |> add(" ") |> nest(~indent=0, e)
      }
    )
  }

  switch e {
  | EInteger(_, i) => b |> add(Int64.to_string(i))
  | EBool(_, true) => b |> add("true")
  | EBool(_, false) => b |> add("false")
  | ENull(_) => b |> add("null")
  | EFloat(_, f) => b |> add(Float.to_string(f))
  | EBlank(_) => b |> add("___")
  | ELet(_, lhs, rhs, next) =>
    b
    |> add("let ")
    |> add(lhs)
    |> add(" ")
    |> addNested(~f=toTokens'(rhs))
    |> addNewlineIfNeeded
    |> addNested(~f=toTokens'(next))
  | ECharacter(_, c) => b |> add("'") |> add(c) |> add("'")
  | EString(_, str) => b |> add("\"") |> add(str) |> add("\'")

  | EIf(_, cond, if', else') =>
    b
    |> add("if ")
    |> addNested(~f=toTokens'(cond))
    |> addNewlineIfNeeded
    |> add("then")
    |> addNewlineIfNeeded
    |> nest(~indent=2, if')
    |> addNewlineIfNeeded
    |> add("else")
    |> add("\n")
    |> nest(~indent=2, else')
  | EFQFnValue(_, name) => b |> add(FQFnName.toString(name))
  | EApply(_, e, args, _, _) =>
    b |> add("(") |> addNested(~f=toTokens'(e)) |> addArgs(args) |> add(")")
  | EConstructor(_, name, exprs) => b |> add(name) |> addArgs(exprs)
  | EFieldAccess(_, expr, fieldname) =>
    b |> addNested(~f=toTokens'(expr)) |> addMany(list{".", fieldname})
  | EVariable(_, name) => b |> add(name)
  | ELambda(_, names, body) =>
    let isLast = i => i == List.length(names) - 1
    b
    |> add("\\")
    |> addIter(names, ~f=(i, (_, name), b) =>
      b |> add(name) |> addIf(!isLast(i), ",") |> addIf(!isLast(i), " ")
    )
    |> add("->")
    |> nest(~indent=2, body)
  | EList(_, exprs) =>
    /* With each iteration of the list, we calculate the new line length if
     * we were to add this new item. If the new line length exceeds the
     * limit, then we add a new line token and an indent by 1 first, before
     * adding the tokenized item to the builder. */
    let lastIndex = List.length(exprs) - 1
    let xOffset = b.xPos |> Option.unwrap(~default=0)
    b
    |> add("[")
    |> addIter(exprs, ~f=(i, e, b') => {
      let currentLineLength = {
        let commaWidth = if i != lastIndex {
          1
        } else {
          0
        }
        toTokens'(e, b').xPos
        |> Option.map(~f=x => x - xOffset + commaWidth)
        |> Option.unwrap(~default=commaWidth)
      }

      // Even if first element overflows, don't put it in a new line
      let isOverLimit = i > 0 && currentLineLength > listLimit
      // Indent after newlines to match the '[ '
      let indent = if isOverLimit {
        1
      } else {
        0
      }
      b'
      |> addIf(isOverLimit, "\n")
      |> indentBy(~indent, ~f=b' => b' |> addNested(~f=toTokens'(e)) |> addIf(i != lastIndex, ", "))
    })
    |> add("]")
  | ETuple(_, first, second, theRest) =>
    let exprs = list{first, second, ...theRest}

    // With each item of the tuple, we calculate the new line length if we were
    // to add this new item. If the new line length exceeds the limit, then we
    // add a new line token and an indent by 1 first, before adding the
    // tokenized item to the builder.

    // TODO or we could define isLastElement, and pass the itemIndex as a param
    // (I think this would be make the code more clear.)
    let lastIndex = List.length(exprs) - 1

    let xOffset = b.xPos |> Option.unwrap(~default=0)

    b
    |> add("(")
    |> addIter(exprs, ~f=(i, e, b') => {
      let currentLineLength = {
        let commaWidth = if i != lastIndex {
          1
        } else {
          0
        }
        toTokens'(e, b').xPos
        |> Option.map(~f=x => x - xOffset + commaWidth)
        |> Option.unwrap(~default=commaWidth)
      }

      let isNotFirstElement = i > 0
      let isOverLimit = currentLineLength > tupleLimit

      // Even if first element overflows, don't put it in a new line
      let shouldIndent = isNotFirstElement && isOverLimit

      // Indent after newlines to match the '( '
      let indent = if shouldIndent {
        1
      } else {
        0
      }

      b'
      |> addIf(shouldIndent, "\n")
      |> indentBy(~indent, ~f=b' => b' |> addNested(~f=toTokens'(e)) |> addIf(i != lastIndex, ", "))
    })
    |> add(")")
  | ERecord(_, fields) =>
    if fields == list{} {
      b |> add("{}")
    } else {
      b
      |> add("{")
      |> indentBy(~indent=2, ~f=b =>
        addIter(fields, b, ~f=(_, (fieldName, expr), b) => {
          b |> addNewlineIfNeeded |> add(fieldName) |> add(": ") |> addNested(~f=toTokens'(expr))
        })
      )
      |> addMany(list{"\n", "}"})
    }
  | EMatch(id, mexpr, pairs) =>
    b
    |> add("match")
    |> addNested(~f=toTokens'(mexpr))
    |> indentBy(~indent=2, ~f=b =>
      b
      |> addIter(pairs, ~f=(i, (pattern, expr), b) =>
        b
        |> addNewlineIfNeeded
        |> addMany(patternToTokens(id, pattern, ~idx=i))
        |> add(" -> ")
        |> addNested(~f=toTokens'(expr))
      )
      |> addNewlineIfNeeded
    )
  | EFeatureFlag(_, cond, _disabled, enabled) =>
    b
    |> add("when")
    |> addNested(~f=toTokens'(cond))
    |> addNewlineIfNeeded
    |> add("enabled")
    |> addNewlineIfNeeded
    |> nest(~indent=2, enabled)
  }
}

let eToHumanString = (e: RuntimeTypes.Expr.t) => {
  toTokens'(e, Builder.empty) |> Builder.toString
}
