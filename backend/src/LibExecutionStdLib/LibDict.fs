module LibExecutionStdLib.LibDict

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.VendoredTablecloth

open LibExecution.RuntimeTypes

module Interpreter = LibExecution.Interpreter
module Errors = LibExecution.Errors
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"


let fns : List<BuiltInFn> =
  [ { name = fn "Dict" "singleton" 0
      parameters = [ Param.make "key" TStr ""; Param.make "value" varA "" ]
      returnType = TDict varA
      description =
        "Returns a dictionary with a single entry {{<param key>: <param value>}}"
      fn =
        (function
        | _, [ DStr k; v ] -> Ply(DObj(Map.ofList [ (k, v) ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "size" 0
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TInt
      description = "Returns the number of entries in <param dict>"
      fn =
        (function
        | _, [ DObj o ] -> Ply(DInt(int64 (Map.count o)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "keys" 0
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList TStr)
      description =
        "Returns <param dict>'s keys in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, [ DObj o ] ->
          o
          |> Map.keys
          |> Seq.map (fun k -> DStr k)
          |> Seq.toList
          |> fun l -> DList l
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "values" 0
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList varA)
      description =
        "Returns <param dict>'s values in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, [ DObj o ] -> o |> Map.values |> Seq.toList |> (fun l -> DList l |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "toList" 0
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList varA)
      description =
        "Returns <param dict>'s entries as a list of {{[key, value]}} lists, in an arbitrary order. This function is the opposite of <fn Dict::fromList>"
      fn =
        (function
        | _, [ DObj o ] ->
          Map.toList o
          |> List.map (fun (k, v) -> DList [ DStr k; v ])
          |> DList
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "fromListOverwritingDuplicates" 0
      parameters = [ Param.make "entries" (TList varA) "" ]
      returnType = TDict varA
      description =
        "Returns a <type dict> with <param entries>. Each value in <param entries>
         must be a {{[key, value]}} list, where <var key> is a <type String>.

         If <param entries> contains duplicate <var key>s, the last entry with that
         key will be used in the resulting dictionary (use <fn Dict::fromList> if you
         want to enforce unique keys).

         This function is the opposite of <fn Dict::toList>."
      fn =
        (function
        | state, [ DList l ] ->

          let f acc e =
            match e with
            | DList [ DStr k; value ] -> Map.add k value acc
            | DList [ k; value ] ->
              Exception.raiseCode (Errors.argumentWasnt "a string" "key" k)
            | (DIncomplete _
            | DErrorRail _
            | DError _) as dv -> Errors.foundFakeDval dv
            | _ -> Exception.raiseCode "All list items must be `[key, value]`"

          let result = List.fold Map.empty f l
          Ply(DObj result)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "fromList" 0
      parameters = [ Param.make "entries" (TList varA) "" ]
      returnType = TOption(TDict varA)
      description =
        "Each value in <param entries> must be a {{[key, value]}} list, where <var
         key> is a <type String>.

         If <param entries> contains no duplicate keys, returns {{Just <var dict>}}
         where <var dict> has <param entries>.

         Otherwise, returns {{Nothing}} (use <fn Dict::fromListOverwritingDuplicates>
         if you want to overwrite duplicate keys)."
      fn =
        (function
        | _, [ DList l ] ->
          let f acc e =
            match acc, e with
            | Some acc, DList [ DStr k; value ] when Map.containsKey k acc -> None
            | Some acc, DList [ DStr k; value ] -> Some(Map.add k value acc)
            | _,
              ((DIncomplete _
              | DErrorRail _
              | DError _) as dv) -> Errors.foundFakeDval dv
            | Some _, DList [ k; _ ] ->
              Exception.raiseCode (Errors.argumentWasnt "a string" "key" k)
            | Some _, _ ->
              Exception.raiseCode "All list items must be `[key, value]`"
            | None, _ -> None

          let result = List.fold (Some Map.empty) f l

          match result with
          | Some map -> Ply(DOption(Some(DObj(map))))
          | None -> Ply(DOption None)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "get" 0
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TStr "" ]
      returnType = varA
      description =
        "Looks up <param key> in <param dict> and returns the value if found, and an error otherwise"
      fn =
        (function
        | _, [ DObj o; DStr s ] ->
          (match Map.tryFind s o with
           | Some d -> Ply(d)
           | None -> Ply(DNull))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "get" 1) }


    { name = fn "Dict" "get" 1
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TStr "" ]
      returnType = TOption varA
      description =
        "Looks up <param key> in <param dict> and returns an <type Option>"
      fn =
        (function
        | _, [ DObj o; DStr s ] -> Ply(DOption(Map.tryFind s o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "get" 2) }


    { name = fn "Dict" "get" 2
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TStr "" ]
      returnType = TOption varA
      description =
        "If the <param dict> contains <param key>, returns the corresponding value,
         wrapped in an <type option>: {{Just value}}. Otherwise, returns {{Nothing}}."
      fn =
        (function
        | _, [ DObj o; DStr s ] -> Map.tryFind s o |> Dval.option |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "member" 0
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TStr "" ]
      returnType = TBool
      description =
        "Returns {{true}} if the <param dict> contains an entry with <param key>, and
         {{false}} otherwise"
      fn =
        (function
        | _, [ DObj o; DStr s ] -> Ply(DBool(Map.containsKey s o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "foreach" 0
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TDict varB
      description =
        "Returns a <type Dict> that contains the same keys as the original <param
         dict> with values that have been transformed by {{fn}}, which operates on
         each value."
      fn =
        (function
        | state, [ DObj o; DFnVal b ] ->
          uply {
            let! result =
              Ply.Map.mapSequentially
                (fun dv ->
                  Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail)
                o

            return DObj result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "map" 0) }


    { name = fn "Dict" "map" 0
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs "fn" (TFn([ TStr; varA ], varB)) "" [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Returns a new dictionary that contains the same keys as the original <param
         dict> with values that have been transformed by {{fn}}, which operates on
         each key-value pair.

         Consider <fn Dict::filterMap> if you also want to drop some of the entries."
      fn =
        (function
        | state, [ DObj o; DFnVal b ] ->
          uply {
            let mapped = Map.mapWithIndex (fun i v -> (i, v)) o

            let! result =
              Ply.Map.mapSequentially
                (fun ((key, dv) : string * Dval) ->
                  Interpreter.applyFnVal
                    state
                    (id 0)
                    b
                    [ DStr key; dv ]
                    NotInPipe
                    NoRail)
                mapped

            return DObj result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "filter" 0
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs "fn" (TFn([ TStr; varA ], TBool)) "" [ "key"; "value" ] ]
      returnType = TDict varA
      description =
        "Calls <param fn> on every entry in <param dict>, returning a <type dict> of
         only those entries for which {{fn key value}} returns {{true}}.

         Consider <fn Dict::filterMap> if you also want to transform the entries."
      fn =
        (function
        | state, [ DObj o; DFnVal b ] ->
          uply {
            let incomplete = ref false

            let f (key : string) (dv : Dval) : Ply<bool> =
              uply {
                let! result =
                  Interpreter.applyFnVal
                    state
                    (id 0)
                    b
                    [ DStr key; dv ]
                    NotInPipe
                    NoRail

                match result with
                | DBool b -> return b
                | DIncomplete _ ->
                  incomplete.Value <- true
                  return false
                | v ->
                  return Exception.raiseCode (Errors.expectedLambdaType "fn" TBool v)
              }

            if incomplete.Value then
              return DIncomplete SourceNone (*TODO(ds) source info *)
            else
              let! result = Ply.Map.filterSequentially f o
              return DObj result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "filter" 1) }


    { name = fn "Dict" "filter" 1
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs "fn" (TFn([ TStr; varA ], TBool)) "" [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Evaluates {{fn key value}} on every entry in <param dict>. Returns a <type
         dict> that contains only the entries of <param dict> for which <param fn>
         returned {{true}}."
      fn =
        (function
        | state, [ DObj o; DFnVal b ] ->
          uply {
            let filter_propagating_errors
              (acc : Result<DvalMap, Dval>)
              (key : string)
              (data : Dval)
              : Ply<Result<DvalMap, Dval>> =
              match acc with
              | Error dv -> Ply(Error dv)
              | Ok m ->
                uply {
                  let! result =
                    Interpreter.applyFnVal
                      state
                      (id 0)
                      b
                      [ DStr key; data ]
                      NotInPipe
                      NoRail

                  match result with
                  | DBool true -> return Ok(Map.add key data m)
                  | DBool false -> return Ok m
                  | (DIncomplete _ as e)
                  | (DError _ as e) -> return Error e
                  | other ->
                    return
                      Exception.raiseCode (
                        Errors.expectedLambdaType "fn" TBool other
                      )
                }

            let! filtered_result =
              Ply.Map.foldSequentially filter_propagating_errors (Ok Map.empty) o

            match filtered_result with
            | Ok o -> return DObj o
            | Error dv -> return dv
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "filterMap" 0
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn([ TStr; varA ], TOption varB))
            ""
            [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Calls <param fn> on every entry in <param dict>, returning a <type dict> that drops some entries (filter) and transforms others (map).
          If {{fn key value}} returns {{Nothing}}, does not add <var key> or <var value> to the new dictionary, dropping the entry.
          If {{fn key value}} returns {{Just newValue}}, adds the entry <var key>: <var newValue> to the new dictionary.
          This function combines <fn Dict::filter> and <fn Dict::map>."
      fn =
        (function
        | state, [ DObj o; DFnVal b ] ->
          uply {
            let abortReason = ref None

            let f (key : string) (data : Dval) : Ply<Dval option> =
              uply {
                let run = abortReason.Value = None

                if run then
                  let! result =
                    Interpreter.applyFnVal
                      state
                      (id 0)
                      b
                      [ DStr key; data ]
                      NotInPipe
                      NoRail

                  match result with
                  | DOption (Some o) -> return Some o
                  | DOption None -> return None
                  | (DIncomplete _
                  | DErrorRail _
                  | DError _) as dv ->
                    abortReason.Value <- Some dv
                    return None
                  | v ->
                    Exception.raiseCode (
                      Errors.expectedLambdaType "fn" (TOption varB) v
                    )

                    return None

                else
                  return None
              }

            let! result = Ply.Map.filterMapSequentially f o

            match abortReason.Value with
            | None -> return DObj result
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "empty" 0
      parameters = []
      returnType = TDict varA
      description = "Returns an empty dictionary"
      fn =
        (function
        | _, [] -> Ply(DObj Map.empty)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "isEmpty" 0
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TBool
      description = "Returns {{true}} if the <param dict> contains no entries"
      fn =
        (function
        | _, [ DObj dict ] -> Ply(DBool(Map.isEmpty dict))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "merge" 0
      parameters =
        [ Param.make "left" (TDict varA) ""; Param.make "right" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Returns a combined dictionary with both dictionaries' entries. If the same key exists in both <param left> and <param right>, it will have the value from <param right>."
      fn =
        (function
        | _, [ DObj l; DObj r ] -> Ply(DObj(Map.mergeFavoringRight l r))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // TODO: move to LibJson
    { name = fn "Dict" "toJSON" 0
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TStr
      description = "Returns <param dict> as a JSON string"
      fn =
        (function
        | _, [ DObj o ] ->
          DObj o |> DvalReprLegacyExternal.toPrettyMachineJsonStringV1 |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "set" 0
      parameters =
        [ Param.make "dict" (TDict(TVariable "a")) ""
          Param.make "key" TStr ""
          Param.make "val" varA "" ]
      returnType = (TDict(TVariable "a"))
      description =
        "Returns a copy of <param dict> with the <param key> set to <param val>"
      fn =
        (function
        | _, [ DObj o; DStr k; v ] -> Ply(DObj(Map.add k v o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "remove" 0
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TStr "" ]
      returnType = TDict varA
      description =
        "If the <param dict> contains <param key>, returns a copy of <param dict> with <param key> and its associated value removed. Otherwise, returns <param dict> unchanged."
      fn =
        (function
        | _, [ DObj o; DStr k ] -> Ply(DObj(Map.remove k o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
