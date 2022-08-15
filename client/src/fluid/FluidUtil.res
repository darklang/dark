open Prelude

let propsFromModel = (m: AppTypes.model): Types.fluidProps => {
  functions: m.functions,
  variants: m.tests,
}

let orderRangeFromSmallToBig = ((rangeBegin, rangeEnd): (int, int)): (int, int) =>
  if rangeBegin > rangeEnd {
    (rangeEnd, rangeBegin)
  } else {
    (rangeBegin, rangeEnd)
  }

@ocaml.doc("Always returns a selection represented as two ints with the smaller
  int first. The numbers are identical if there is no selection.")
let getSelectionRange = (s: AppTypes.fluidState): (int, int) =>
  switch s.selectionStart {
  | Some(beginIdx) if beginIdx < s.newPos => (beginIdx, s.newPos)
  | Some(endIdx) => (s.newPos, endIdx)
  | None => (s.newPos, s.newPos)
  }

@ocaml.doc(" [truncateStringTo64BitInt s] attempts to chop off the
 * least-significant base-10 digits of a string [s] intended
 * to represent an integer, until the resulting truncated string
 * is a valid base-10 representation of a 64bit integer.
 * If it succeeds, it produces (Ok truncatedString).
 * If the result after truncation is not a valid base-10 encoding
 * of a 64 bit integer, it returns an Error.
 *
 * FIXME: This returns Error for strings with leading 0s.
 *
 * TODO: This only supports positive numbers for now, but we should change this
 * once fluid supports negative numbers.
 ")
let truncateStringTo64BitInt = (s: string): Result.t<int64, string> => {
  let is63BitInt = s =>
    switch Native.BigInt.asUintN(~nBits=63, s) {
    | Some(i) => Native.BigInt.toString(i) == s
    | None => false
    }

  // 9223372036854775807 is largest positive 63 bit number, which has 19 characters
  // We use 63 bit checks instead of 64 because the most significanty bit is for sign
  // in two's complement -- which is not yet handled
  let trunc19 = String.left(~count=19, s)
  if is63BitInt(trunc19) {
    Ok(Int64.of_string(trunc19))
  } else {
    let trunc18 = String.left(~count=18, s)
    if is63BitInt(trunc18) {
      Ok(Int64.of_string(trunc18))
    } else {
      Error("Invalid 63bit number even after truncate")
    }
  }
}

@ocaml.doc(" [coerceStringTo64BitInt s] produces a string
 * representing a 63-bit base-10 integer based on the
 * input string [s], truncating the least significant
 * base-10 digits if necessary.
 *
 * Unhandled representations produce \"0\" as output.
 *
 * FIXME: This returns \"0\" for strings with leading 0s due to truncateStringTo63BitInt.
 *
 * TODO: This only supports positive numbers for now, but we should change this
 * (and probably support 64 bit integers) once fluid supports negative numbers.
 ")
let coerceStringTo64BitInt = (s: string): int64 =>
  Result.unwrap(truncateStringTo64BitInt(s), ~default=0L)

let trimQuotes = (v): string => {
  open String
  // hmmm is there any reason the original code had all of those pipes?

  // trim start
  let v =
    if endsWith(~suffix="\"", v) {
      dropRight(~count=1, v)
    } else {
      v
    }

  // trim end
  if startsWith(~prefix="\"", v) {
    dropLeft(~count=1, v)
  } else {
    v
  }
}

let removeCharAt = (str, offset): string =>
  if offset < 0 {
    str
  } else {
    String.slice(~from=0, ~to_=offset, str) ++
    String.slice(~from=offset + 1, ~to_=String.length(str), str)
  }

let isNumber = (str: string) => Js.Re.test_(%re("/^[0-9]+$/"), str)

let isIdentifierChar = (str: string) => Js.Re.test_(%re("/[_a-zA-Z0-9]+/"), str)

@ocaml.doc(
  " [isValidIdentifier str] tests if [str] is a valid identifier for things like variable names "
)
let isValidIdentifier = (str: string): bool => Js.Re.test_(%re("/^[_a-zA-Z]+[_a-zA-Z0-9]*$/"), str)

/* isUnicodeLetter returns true if the string consists only of unicode letters
 * \p{L} is the unicode character class for letters */
let isUnicodeLetter = (str: string): bool => Js.Re.test_(%re("/^\\p{L}+$/u"), str)

/* Checks if this is allowed as a fieldName in a record literal. Note
 * that there is a difference between what we allow in record
 * literals and field accesses: we decided to allow '-' in record
 * field definitions since they are used for HTTP headers a lot.
 * However, typing '-' in a field access can conflict with the infix
 * '-', so this function should not be used for field access. */
let isValidRecordLiteralFieldName = (str: string): bool =>
  Js.Re.test_(%re("/^[_a-zA-Z]+[-_a-zA-Z0-9]*$/"), str)

let isFnNameChar = str => Js.Re.test_(%re("/[_:a-zA-Z0-9]/"), str) && String.length(str) == 1

let splitFnName = (fnName: string): (option<string>, string, string) => {
  let pattern = Js.Re.fromString("^((\\w+)::)?([^_]+)(_v(\\d+))?$")
  let mResult = Js.Re.exec_(pattern, fnName)
  switch mResult {
  | Some(result) =>
    let captures = result |> Js.Re.captures |> Belt.List.fromArray |> List.map(~f=Js.toOption)

    switch captures {
    | list{_, _, mod_, Some(fn), _, Some(v)} => (mod_, fn, v)
    | list{_, _, mod_, Some(fn), _, None} => (mod_, fn, "0")
    | _ => recover("invalid fn name", ~debug=fnName, (None, fnName, "0"))
    }
  | None => (None, fnName, "0")
  }
}

// Get just the function mod and name
let fnDisplayName = (fnName: string): string => {
  let (mod_, name, _) = splitFnName(fnName)
  switch mod_ {
  | Some(mod_) => mod_ ++ ("::" ++ name)
  | None => name
  }
}

// Get just the function version
let versionDisplayName = (fnName: string): string => {
  let (_, _, version) = splitFnName(fnName)
  if version == "0" {
    ""
  } else {
    "v" ++ version
  }
}

let partialName = fnDisplayName

let ghostPartialName = (fnName: string) => partialName(fnName) ++ versionDisplayName(fnName)

let fnDisplayNameWithVersion = (fnName: string) => partialName(fnName) ++ versionDisplayName(fnName)
