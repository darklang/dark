/// The package manager allows user-defined functions to be shared with other
/// users. Currently only enabled for admins.
module LibBackend.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude
open Tablecloth
open Db

module BinarySerialization = LibBinarySerialization.BinarySerialization
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes

// ------------------
// Uploading
// ------------------

// exception InvalidFunction of string
//
// let extract_metadata (fn : RuntimeT.user_fn) :
//     string * parameter list * RuntimeT.tipe * string =
//   let name =
//     match fn.metadata.name with
//     | Filled (_, name) ->
//         name
//     | _ ->
//         raise (InvalidFunction "Missing function name")
//   in
//   let parameters : parameter list =
//     List.map fn.metadata.parameters ~f:(fun p ->
//         match (p.name, p.tipe) with
//         | Filled (_, name), Filled (_, tipe) ->
//             ({name; tipe; description = p.description} : parameter)
//         | Filled (_, name), _ ->
//             raise (InvalidFunction "Missing tipe for function parameter")
//         | _, Filled (_, tipe) ->
//             raise (InvalidFunction "Missing name for function parameter")
//         | _, _ ->
//             raise
//               (InvalidFunction "Invalid name and tipe for function parameter"))
//   in
//   let return_type =
//     match fn.metadata.return_type with
//     | Filled (_, return_type) ->
//         return_type
//     | _ ->
//         raise (InvalidFunction "Invalid return type")
//   in
//   (name, parameters, return_type, fn.metadata.description)
//
//
// let parse_fnname (name : string) : string * string * string * string * int =
//   let regex_of str = Re2.create_exn ("^" ^ str ^ "$") in
//   let user_pattern = "[a-z][a-z0-9_]+" in
//   let package_pattern = "[a-z][a-z0-9A-Z]+" in
//   let module_pattern = "[A-Z][a-z0-9A-Z_]+" in
//   let fnname_pattern = "[a-z][a-z0-9A-Z_]+" in
//   let version_pattern = "[0-9]+" in
//   let toplevel_regex =
//     (* regexes same as libbackend/account.ml and client/Autocomplete.ml *)
//     regex_of "(.*)/(.*)/(.*)::(.*)_v(.*)"
//   in
//   let results =
//     name
//     |> Re2.get_matches ~max:1 toplevel_regex
//     |> Or_error.ok
//     |> Option.bind ~f:List.hd
//     |> Option.map ~f:Re2.without_trailing_none
//     |> Option.map ~f:Re2.Match.get_all
//   in
//   let must_match (pattern : string) (value : string) (help_name : string) =
//     if Re2.matches (regex_of pattern) value
//     then value
//     else
//       raise
//         (InvalidFunction
//            ("Invalid function name: " ^ help_name ^ " should match " ^ pattern))
//   in
//   match results with
//   | Some [|_; Some user; Some package; Some module_; Some fnname; Some version|]
//     ->
//       let user = must_match user_pattern user "user_or_org" in
//       let package = must_match package_pattern package "package" in
//       let module_ = must_match module_pattern module_ "module_" in
//       let fnname = must_match fnname_pattern fnname "fnname" in
//       let version = must_match version_pattern version "version" in
//       (user, package, module_, fnname, Int.of_string version)
//   | other ->
//       raise
//         (InvalidFunction
//            "Invalid function name, missing part of the name. It should match {user_or_org}/{package}/{module}::{fnname}_v{number}")
//
//
// type expr_tlid_pair =
//   { tlid : Serialization_format.tlid
//   ; expr : Serialization_format.RuntimeT.expr }
// [@@deriving bin_io, show]
//
// type expr_tlid_pair = Types.fluid_expr * tlid [@@deriving show]
//
// let expr_tlid_pair_to_binary_string ((expr, tlid) : expr_tlid_pair) : string =
//   let expr = Serialization_converters.fromFluidExpr expr in
//   {expr; tlid}
//   |> Core_extended.Bin_io_utils.to_line bin_expr_tlid_pair
//   |> Bigstring.to_string
//
//
// let binary_string_to_expr_tlid_pair (str : string) : expr_tlid_pair =
//   let {expr; tlid} =
//     Core_extended.Bin_io_utils.of_line str bin_expr_tlid_pair
//   in
//   (Serialization_converters.toFluidExpr expr, tlid)
//
//
// let function_name (fn : fn) : string =
//   fn.module_ ^ "::" ^ fn.fnname ^ "_v" ^ string_of_int fn.version
//
//
// let max_version
//     (function_name : string)
//     (username : string)
//     (package : string)
//     (module_ : string)
//     (fnname : string) : int option =
//   Db.run
//     ~name:"add_package_management_function abort txn if exn"
//     "ABORT"
//     ~params:[] ;
//   Db.fetch_one
//     ~name:"add_package_management_function get_latest_version_for_error"
//     "SELECT MAX(version) FROM packages_v0 JOIN accounts ON user_id = accounts.id
//                   WHERE username = $1 AND package = $2 AND module = $3 AND fnname = $4"
//     ~subject:function_name
//     ~params:
//       [ Db.String username
//       ; Db.String package
//       ; Db.String module_
//       ; Db.String fnname ]
//     ~result:TextResult
//   |> List.hd
//   |> Option.bind ~f:(fun v -> if v = "" then None else Some (int_of_string v))
//
//
// let add_function (fn : fn) : unit =
//   let user =
//     fn.user
//     |> Account.userIDForUserName
//     |> Option.value_exn ~message:"Invalid package owner"
//   in
//   let author =
//     Account.userIDForUserName fn.author
//     |> Option.value_exn ~message:"Invalid author"
//   in
//   let existing_version =
//     max_version (function_name fn) fn.user fn.package fn.module_ fn.fnname
//   in
//   (* Here's the deal: we have 3 cases:
//    * - no existing version, we'll work with any non-negative version   you
//    * provided
//    * - new version is one greater than existing version, it works
//    * - new version is _not_  one greater than existing version; we'll attemt to
//    * insert a fn with version=existing version because that violates our
//    * dupe_key constraint in the DB.
//    *
//    * We do it this way and lean on the db constraint, to keep it all in
//    * transaction, avoid race conditions, and handle all failures to insert in
//    * one place *)
//   let version =
//     match existing_version with
//     | None ->
//         fn.version
//     | Some existing_version ->
//         if fn.version = existing_version + 1
//         then fn.version
//         else existing_version
//   in
//   Db.transaction ~name:"add_package_management_function begin" (fun () ->
//       (* After insert, also auto-deprecate any previous versions of fn *)
//       Db.run
//         ~name:"add_package_management_function insert"
//         "INSERT INTO packages_v0 (tlid, user_id, package, module, fnname, version,
//                                   description, body, return_type, parameters, author_id, deprecated)
//          VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10::jsonb, $11, $12)"
//         ~subject:(function_name fn)
//         ~params:
//           [ Db.Int63 fn.tlid
//           ; Db.Uuid user
//           ; Db.String fn.package
//           ; Db.String fn.module_
//           ; Db.String fn.fnname
//           ; Db.Int version
//           ; Db.String fn.description
//           ; Db.Binary (expr_tlid_pair_to_binary_string (fn.body, fn.tlid))
//           ; Db.String (Dval.tipe_to_string fn.return_type)
//           ; Db.String
//               (fn.parameters |> parameters_to_yojson |> Yojson.Safe.to_string)
//           ; Db.Uuid author
//           ; Db.Bool fn.deprecated ]
//         ~result:TextResult ;
//       (* Note: 'AND deprecated = false' is kind of a no-op, but it's slightly more
//        * elegant in that the rows-affected count is more meaningful, not that we use
//        * that currently *)
//       Db.run
//         ~name:"add_package_management_function deprecate old versions"
//         "UPDATE packages_v0
//      SET deprecated = true
//      WHERE user_id = $1
//      AND package = $2
//      AND module = $3
//      AND fnname = $4
//      AND version < $5
//      AND deprecated = false"
//         ~subject:(function_name fn)
//         ~params:
//           [ Db.Uuid user
//           ; Db.String fn.package
//           ; Db.String fn.module_
//           ; Db.String fn.fnname
//           ; Db.Int fn.version ]
//         ~result:TextResult)
//
//
// let save (author : string) (fn : RuntimeT.user_fn) : (unit, string) Result.t =
//   (* First let's be very sure we have a correct function *)
//   let metadata =
//     try extract_metadata fn |> Result.return
//     with InvalidFunction msg -> Error msg
//   in
//   let parsed_data =
//     metadata
//     |> Result.bind ~f:(fun metadata ->
//            try
//              let name, parameters, return_type, description = metadata in
//              (metadata, parse_fnname name) |> Result.return
//              (* Binary values can only be fetched on their own, so we create an ID and
//               * encode it within the binary-serialized value, so we know what function
//               * it belongs to when fetching back later.  Uuids would be better but
//               * don't serialize using bin_io. *)
//            with InvalidFunction msg -> Error msg)
//   in
//   parsed_data
//   |> Result.bind ~f:(fun parsed_data ->
//          let metadata, (user, package, module_, fnname, version) =
//            parsed_data
//          in
//          let name, parameters, return_type, description = metadata in
//          let tlid = Util.create_id () in
//          try
//            add_function
//              { user
//              ; tlid
//              ; package
//              ; module_
//              ; fnname
//              ; version
//              ; body = fn.ast
//              ; return_type
//              ; parameters
//              ; description
//              ; author
//              ; deprecated = false } ;
//            Ok ()
//          with
//          | Exception.DarkException {tipe = DarkStorage; short; _}
//            when Tc.String.contains ~substring:"duplicate key" short ->
//              let max_version =
//                max_version name user package module_ fnname
//                |> Option.value ~default:0
//              in
//              Error
//                (Printf.sprintf
//                   "Function already exists with this name and versions up to %i, try version %i?"
//                   max_version
//                   (max_version + 1))
//          | e ->
//              (* Error "Problem saving function" *)
//              Error ("Unknown error: " ^ Exception.to_string e))

let writeBody2 (tlid : tlid) (expr : PT.Expr) : Task<unit> =
  task {
    let binary = BinarySerialization.serializeExpr tlid expr
    return!
      Sql.query "UPDATE packages_v0 SET body = @body where tlid = @tlid"
      |> Sql.parameters [ "body", Sql.bytea binary; "tlid", Sql.tlid tlid ]
      |> Sql.executeStatementAsync
  }


// ------------------
// Fetching functions
// ------------------


type Parameter = { name : string; typ : PT.TypeReference; description : string }

let parameter2PT (o : Parameter) : PT.Package.Parameter =
  { name = o.name; description = o.description; typ = o.typ }

type ParametersDBFormat = List<Parameter>

let allFunctions () : Task<List<PT.Package.Fn>> =
  task {
    let! fns =
      // CLEANUP: why do we have both accounts_v0 A and accounts_v0 O?
      Sql.query
        "SELECT P.tlid, P.user_id, P.package, P.modules, P.fnname, P.version,
                P.body, P.description, P.return_type, P.parameters, P.deprecated
          FROM packages_v0 P, accounts_v0 A, accounts_v0 O
          WHERE P.user_id = A.id
            AND P.author_id = O.id"
      |> Sql.parameters []
      |> Sql.executeAsync (fun read ->
        (read.string "username",
         read.string "package",
         read.string "modules",
         read.string "fnname",
         read.int "version",
         read.bytea "body",
         read.string "return_type",
         read.string "parameters",
         read.string "description",
         read.string "author",
         read.bool "deprecated",
         read.tlid "tlid"))

    return
      fns
      |> List.map
        (fun (username,
              package,
              modules,
              fnname,
              version,
              body2,
              returnType,
              parameters,
              description,
              author,
              deprecated,
              tlid) ->
          let name : PT.FQFnName.PackageFnName =
            { owner = username
              package = package
              modules = Json.Vanilla.deserialize<NonEmptyList<string>> modules
              function_ = fnname
              version = version }
          let expr = BinarySerialization.deserializeExpr tlid body2
          let parameters =
            parameters
            |> Json.Vanilla.deserialize<ParametersDBFormat>
            |> List.map parameter2PT
          let returnType = returnType |> Json.Vanilla.deserialize<PT.TypeReference>
          { name = name
            typeParams = [] // CLEANUP
            parameters = parameters
            returnType = returnType
            body = expr
            description = description
            author = author
            deprecated = deprecated
            tlid = tlid })
  }
