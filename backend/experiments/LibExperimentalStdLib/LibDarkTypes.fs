module LibExperimentalStdLib.LibDarkTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module Errors = LibExecution.Errors

open LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

let fn = FQFnName.stdlibFnName
let tp = PT.FQTypeName.stdlibTypeName

let incorrectArgs = Errors.incorrectArgs


// maybe these types/fns are only available to DarkEditor - giving access to users at this point
// is probably more confusing than it's worth

// TODO: separately, write a fuzzer that will randomly generate these types

// TODO: a bunch of stuff is missing
let types : List<PT.BuiltInType> =
  [ { name = tp "ProgramTypes" "TypeReference" 0
      typeParams = [ ]
      definition =
        PT.CustomType.Enum(
          { id = gid(); name = "TInt"; fields = [] },
          [ { id = gid(); name = "TString"; fields = [] }
            { id = gid(); name = "TCustomType"; fields = [] } ]
        )
      description = "Represents a PT.TypeReference" }


    { name = tp "ProgramTypes" "Expr" 0
      typeParams = [ ]
      definition =
        PT.CustomType.Enum(
          { id = gid(); name = "EUnit"; fields = [] },
          [
            { id = gid(); name = "EBool"; fields = [{ id = gid(); typ = PT.TBool; label = None }] }

            { id = gid()
              name = "EString"
              // TODO: support interpolation
              fields = [ { id = gid(); typ = PT.TString; label = None } ] }

            { id = gid(); name = "EInt"; fields = [{ id = gid(); typ = PT.TInt; label = None }] }

            { id = gid()
              name = "ELet"
              fields = [
                { id = gid(); typ = PT.TString; label = Some "binding" }
                { id = gid(); typ = PT.TVariable "TODO: ANY"; label = Some "value" }
                { id = gid(); typ = PT.TVariable "TODO: ANY"; label = Some "body"}
              ] }

            { id = gid()
              name = "EVariable"
              fields = [
                { id = gid(); typ = PT.TString; label = None }
              ] } ]
        )
      description = "Represents a PT.Expr" }


      (*
        module UserFunction =
  type Parameter =
    { id : id
      name : string
      typ : TypeReference
      description : string }

  type T =
    { tlid : tlid
      name : string
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      infix : bool
      body : Expr }
      *)


    // { name = tp "ProgramTypes" "UserFunctionParameter" 0
    //   typeParams = [ ]
    //   definition =
    //     PT.CustomType.Record(
    //       { id = gid(); name = "name"; typ : TypeReference }
    //       // name: string
    //       { id = 1UL; name = "name"; typ = PT.TString; label = None },
    //       // typ : TypeReference
    //     )
    //   description = "Represents a PT.UserFunction.Parameter" }

    // { name = tp "ProgramTypes" "UserFunction" 0
    //   typeParams = [ ]
    //   definition =
    //     PT.CustomType.Enum() }
      ]




// // Dark fn that goes from SynTree -> PT.Expr
// // later: Dark fn that goes from SynTree -> PT.TypeReference
// // later: Dark fn that goes from SynTree -> PT.Function

// ... I'm not sure how we could

// //type StdlibTypeName = { module_ : string; typ : string; version : int }

// module Parser =

//   // lol what if this were a Dark fn?
//   let toExpr (node : SynTree.Node) : PT.Expr =
//     let (typ, text, children) = Node node

//     match typ, text, children with
//     | ("Int", i, []) -> ProgramTypes.Expr(typeName, "EConstructor", [int i])
//     | ("String", s, []) -> ProgramTypes.Expr.EString(s)
