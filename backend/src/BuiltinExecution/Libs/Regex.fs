module BuiltinExecution.Libs.Regex

open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


let fns : List<BuiltInFn> =
  [ { name = fn "regexIsMatch" 0
      typeParams = []
      parameters =
        [ Param.make "input" TString "The string to test"
          Param.make "pattern" TString "The regular expression pattern" ]
      returnType = TBool
      description =
        "Tests if <param input> matches the regex <param pattern>. Returns {{true}} if a match is found, {{false}} otherwise."
      fn =
        (function
        | _, _, _, [ DString input; DString pattern ] ->
          try
            let isMatch = Regex.IsMatch(input, pattern)
            Ply(DBool isMatch)
          with :? System.ArgumentException as e ->
            Exception.raiseInternal
              "Invalid regex pattern"
              [ "pattern", pattern; "error", e.Message ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "regexFind" 0
      typeParams = []
      parameters =
        [ Param.make "input" TString "The string to search in"
          Param.make "pattern" TString "The regular expression pattern" ]
      returnType = TypeReference.option TString
      description =
        "Finds the first match of the regex <param pattern> in <param input>. Returns {{Some match}} if found, {{None}} otherwise."
      fn =
        (function
        | _, _, _, [ DString input; DString pattern ] ->
          try
            let m = Regex.Match(input, pattern)
            if m.Success then
              Dval.optionSome KTString (DString m.Value) |> Ply
            else
              Dval.optionNone KTString |> Ply
          with :? System.ArgumentException as e ->
            Exception.raiseInternal
              "Invalid regex pattern"
              [ "pattern", pattern; "error", e.Message ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "regexFindAll" 0
      typeParams = []
      parameters =
        [ Param.make "input" TString "The string to search in"
          Param.make "pattern" TString "The regular expression pattern" ]
      returnType = TList TString
      description =
        "Finds all matches of the regex <param pattern> in <param input>. Returns a list of matched strings."
      fn =
        (function
        | _, _, _, [ DString input; DString pattern ] ->
          try
            let matches = Regex.Matches(input, pattern)
            let results =
              matches
              |> Seq.cast<Match>
              |> Seq.map (fun m -> DString m.Value)
              |> Seq.toList
            Ply(Dval.list KTString results)
          with :? System.ArgumentException as e ->
            Exception.raiseInternal
              "Invalid regex pattern"
              [ "pattern", pattern; "error", e.Message ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "regexReplace" 0
      typeParams = []
      parameters =
        [ Param.make "input" TString "The string to operate on"
          Param.make "pattern" TString "The regular expression pattern"
          Param.make "replacement" TString "The replacement string" ]
      returnType = TString
      description =
        "Replaces the first match of the regex <param pattern> in <param input> with <param replacement>."
      fn =
        (function
        | _, _, _, [ DString input; DString pattern; DString replacement ] ->
          try
            // Only replace first occurrence
            let m = Regex.Match(input, pattern)
            if m.Success then
              let result =
                input.Substring(0, m.Index)
                + replacement
                + input.Substring(m.Index + m.Length)
              Ply(DString result)
            else
              Ply(DString input)
          with
          | :? System.ArgumentException as e ->
            Exception.raiseInternal
              "Invalid regex pattern"
              [ "pattern", pattern; "error", e.Message ]
          | :? RegexMatchTimeoutException ->
            Exception.raiseInternal "Regex timeout" [ "pattern", pattern ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "regexReplaceAll" 0
      typeParams = []
      parameters =
        [ Param.make "input" TString "The string to operate on"
          Param.make "pattern" TString "The regular expression pattern"
          Param.make "replacement" TString "The replacement string" ]
      returnType = TString
      description =
        "Replaces all matches of the regex <param pattern> in <param input> with <param replacement>."
      fn =
        (function
        | _, _, _, [ DString input; DString pattern; DString replacement ] ->
          try
            let result =
              Regex.Replace(
                input,
                pattern,
                replacement,
                RegexOptions.None,
                System.TimeSpan.FromMilliseconds(1000.0)
              )
            Ply(DString result)
          with
          | :? System.ArgumentException as e ->
            Exception.raiseInternal
              "Invalid regex pattern"
              [ "pattern", pattern; "error", e.Message ]
          | :? RegexMatchTimeoutException ->
            Exception.raiseInternal "Regex timeout" [ "pattern", pattern ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "regexSplit" 0
      typeParams = []
      parameters =
        [ Param.make "input" TString "The string to split"
          Param.make "pattern" TString "The regular expression pattern to split on" ]
      returnType = TList TString
      description =
        "Splits <param input> by the regex <param pattern>, returning a list of strings."
      fn =
        (function
        | _, _, _, [ DString input; DString pattern ] ->
          try
            let parts = Regex.Split(input, pattern)
            let results = parts |> Array.map DString |> Array.toList
            Ply(Dval.list KTString results)
          with :? System.ArgumentException as e ->
            Exception.raiseInternal
              "Invalid regex pattern"
              [ "pattern", pattern; "error", e.Message ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
