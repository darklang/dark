module StdLibExecution.Libs.Dict

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth
open LibExecution.StdLib.Shortcuts

module Errors = LibExecution.Errors
module Interpreter = LibExecution.Interpreter

let varA = TVariable "a"
let varB = TVariable "b"

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "Dict" "singleton" 0
      typeParams = []
      parameters = [ Param.make "key" TString ""; Param.make "value" varA "" ]
      returnType = TDict varA
      description =
        "Returns a dictionary with a single entry {{<param key>: <param value>}}"
      fn =
        (function
        | _, _, [ DString k; v ] -> Ply(DDict(Map.ofList [ (k, v) ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "size" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TInt
      description = "Returns the number of entries in <param dict>"
      fn =
        (function
        | _, _, [ DDict o ] -> Ply(DInt(int64 (Map.count o)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "keys" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList TString)
      description =
        "Returns <param dict>'s keys in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, _, [ DDict o ] ->
          o
          |> Map.keys
          |> Seq.map (fun k -> DString k)
          |> Seq.toList
          |> fun l -> DList l
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "values" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList varA)
      description =
        "Returns <param dict>'s values in a <type List>, in an arbitrary order"
      fn =
        (function
        | _, _, [ DDict o ] ->
          o |> Map.values |> Seq.toList |> (fun l -> DList l |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "toList" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = (TList(TTuple(varA, varB, [])))
      description =
        "Returns <param dict>'s entries as a list of {{(key, value)}} tuples, in an arbitrary order. This function is the opposite of <fn Dict.fromList>"
      fn =
        (function
        | _, _, [ DDict o ] ->
          Map.toList o
          |> List.map (fun (k, v) -> DTuple(DString k, v, []))
          |> DList
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "fromListOverwritingDuplicates" 0
      typeParams = []
      parameters = [ Param.make "entries" (TList(TTuple(varA, varB, []))) "" ]
      returnType = TDict varA
      description =
        "Returns a <type dict> with <param entries>. Each value in <param entries>
         must be a {{(key, value)}} tuple, where <var key> is a <type String>.

         If <param entries> contains duplicate <var key>s, the last entry with that
         key will be used in the resulting dictionary (use <fn Dict.fromList> if you
         want to enforce unique keys).

         This function is the opposite of <fn Dict.toList>."
      fn =
        (function
        | _, _, [ DList l ] ->

          let f acc e =
            match e with
            | DTuple (DString k, value, []) -> Map.add k value acc
            | DTuple (k, _, []) ->
              Exception.raiseCode (Errors.argumentWasntType TString "key" k)
            | (DIncomplete _
            | DError _) as dv -> Errors.foundFakeDval dv
            | _ -> Exception.raiseCode "All list items must be `(key, value)`"

          let result = List.fold Map.empty f l
          Ply(DDict result)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "fromList" 0
      typeParams = []
      parameters = [ Param.make "entries" (TList(TTuple(varA, varB, []))) "" ]
      returnType = TOption(TDict varB)
      description =
        "Each value in <param entries> must be a {{(key, value)}} tuple, where <var
         key> is a <type String>.

         If <param entries> contains no duplicate keys, returns {{Just <var dict>}}
         where <var dict> has <param entries>.

         Otherwise, returns {{Nothing}} (use <fn Dict.fromListOverwritingDuplicates>
         if you want to overwrite duplicate keys)."
      fn =
        (function
        | _, _, [ DList l ] ->
          let f acc e =
            match acc, e with
            | Some acc, DTuple (DString k, _, _) when Map.containsKey k acc -> None
            | Some acc, DTuple (DString k, value, []) -> Some(Map.add k value acc)
            | _,
              ((DIncomplete _
              | DError _) as dv) -> Errors.foundFakeDval dv
            | Some _, DTuple (k, _, []) ->
              Exception.raiseCode (Errors.argumentWasntType TString "key" k)
            | Some _, _ ->
              Exception.raiseCode "All list items must be `(key, value)`"
            | None, _ -> None

          let result = List.fold (Some Map.empty) f l

          match result with
          | Some map -> Ply(DOption(Some(DDict(map))))
          | None -> Ply(DOption None)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "get" 2
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TOption varA
      description =
        "If the <param dict> contains <param key>, returns the corresponding value,
         wrapped in an <type Option>: {{Just value}}. Otherwise, returns {{Nothing}}."
      fn =
        (function
        | _, _, [ DDict o; DString s ] -> Map.tryFind s o |> Dval.option |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "member" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TBool
      description =
        "Returns {{true}} if the <param dict> contains an entry with <param key>, and
         {{false}} otherwise"
      fn =
        (function
        | _, _, [ DDict o; DString s ] -> Ply(DBool(Map.containsKey s o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "map" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn([ TString; varA ], varB))
            ""
            [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Returns a new dictionary that contains the same keys as the original <param
         dict> with values that have been transformed by {{fn}}, which operates on
         each key-value pair.

         Consider <fn Dict.filterMap> if you also want to drop some of the entries."
      fn =
        (function
        | state, _, [ DDict o; DFnVal b ] ->
          uply {
            let mapped = Map.mapWithIndex (fun i v -> (i, v)) o

            let! result =
              Ply.Map.mapSequentially
                (fun ((key, dv) : string * Dval) ->
                  Interpreter.applyFnVal state b [ DString key; dv ])
                mapped

            return DDict result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "filter" 1
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn([ TString; varA ], TBool))
            ""
            [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Evaluates {{fn key value}} on every entry in <param dict>. Returns a <type
         dict> that contains only the entries of <param dict> for which <param fn>
         returned {{true}}."
      fn =
        (function
        | state, _, [ DDict o; DFnVal b ] ->
          uply {
            let filterPropagatingErrors
              (acc : Result<DvalMap, Dval>)
              (key : string)
              (data : Dval)
              : Ply<Result<DvalMap, Dval>> =
              match acc with
              | Error dv -> Ply(Error dv)
              | Ok m ->
                uply {
                  let! result = Interpreter.applyFnVal state b [ DString key; data ]

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

            let! filteredResult =
              Ply.Map.foldSequentially filterPropagatingErrors (Ok Map.empty) o

            match filteredResult with
            | Ok o -> return DDict o
            | Error dv -> return dv
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "filterMap" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict varA) ""
          Param.makeWithArgs
            "fn"
            (TFn([ TString; varA ], TOption varB))
            ""
            [ "key"; "value" ] ]
      returnType = TDict varB
      description =
        "Calls <param fn> on every entry in <param dict>, returning a <type dict> that drops some entries (filter) and transforms others (map).
          If {{fn key value}} returns {{Nothing}}, does not add <var key> or <var value> to the new dictionary, dropping the entry.
          If {{fn key value}} returns {{Just newValue}}, adds the entry <var key>: <var newValue> to the new dictionary.
          This function combines <fn Dict.filter> and <fn Dict.map>."
      fn =
        (function
        | state, _, [ DDict o; DFnVal b ] ->
          uply {
            let abortReason = ref None

            let f (key : string) (data : Dval) : Ply<Dval option> =
              uply {
                let run = abortReason.Value = None

                if run then
                  let! result = Interpreter.applyFnVal state b [ DString key; data ]

                  match result with
                  | DOption (Some o) -> return Some o
                  | DOption None -> return None
                  | (DIncomplete _
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
            | None -> return DDict result
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "empty" 0
      typeParams = []
      parameters = []
      returnType = TDict varA
      description = "Returns an empty dictionary"
      fn =
        (function
        | _, _, [] -> Ply(DDict Map.empty)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "isEmpty" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) "" ]
      returnType = TBool
      description = "Returns {{true}} if the <param dict> contains no entries"
      fn =
        (function
        | _, _, [ DDict dict ] -> Ply(DBool(Map.isEmpty dict))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "merge" 0
      typeParams = []
      parameters =
        [ Param.make "left" (TDict varA) ""; Param.make "right" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Returns a combined dictionary with both dictionaries' entries. If the same key exists in both <param left> and <param right>, it will have the value from <param right>."
      fn =
        (function
        | _, _, [ DDict l; DDict r ] -> Ply(DDict(Map.mergeFavoringRight l r))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // TYPESCLEANUP - just a quick hack since we don't have record setting syntax
    { name = fn "Record" "set" 0
      typeParams = []
      parameters =
        [ Param.make "record" (TVariable "a") ""
          Param.make "key" TString ""
          Param.make "val" varA "" ]
      returnType = (TVariable "a")
      description =
        "Returns a copy of <param record> with the <param key> set to <param val>"
      fn =
        (function
        | _, _, [ DRecord o; DString k; v ] -> Ply(DRecord(Map.add k v o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }



    { name = fn "Dict" "set" 0
      typeParams = []
      parameters =
        [ Param.make "dict" (TDict(TVariable "a")) ""
          Param.make "key" TString ""
          Param.make "val" varA "" ]
      returnType = (TDict(TVariable "a"))
      description =
        "Returns a copy of <param dict> with the <param key> set to <param val>"
      fn =
        (function
        | _, _, [ DDict o; DString k; v ] -> Ply(DDict(Map.add k v o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Dict" "remove" 0
      typeParams = []
      parameters = [ Param.make "dict" (TDict varA) ""; Param.make "key" TString "" ]
      returnType = TDict varA
      description =
        "If the <param dict> contains <param key>, returns a copy of <param dict> with <param key> and its associated value removed. Otherwise, returns <param dict> unchanged."
      fn =
        (function
        | _, _, [ DDict o; DString k ] -> Ply(DDict(Map.remove k o))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
