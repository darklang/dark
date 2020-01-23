open Core_kernel
open Libcommon
open Libexecution
open Types

(* ------------------ *)
(* APIs *)
(* ------------------ *)

type parameter =
  { name : string
  ; tipe : RuntimeT.tipe
  ; description : string }
[@@deriving yojson]

type parameters = parameter list [@@deriving yojson]

let runtime_param_of_parameter (p : parameter) : RuntimeT.param =
  { name = p.name
  ; tipe = p.tipe
  ; block_args = []
  ; optional = false
  ; description = p.description }


type fn =
  { user : string
  ; package : string
  ; module_ : string [@key "module"]
  ; fnname : string
  ; version : int
  ; body : RuntimeT.expr
  ; parameters : parameter list
  ; return_type : RuntimeT.tipe
  ; description : string
  ; author : string
  ; deprecated : bool
  ; tlid : tlid }
[@@deriving yojson]

type get_packages_rpc_result = fn list [@@deriving yojson]

let to_name (fn : fn) : string =
  Printf.sprintf
    "%s/%s/%s::%s_v%d"
    fn.user
    fn.package
    fn.module_
    fn.fnname
    fn.version


(* ------------------ *)
(* Uploading *)
(* ------------------ *)
exception InvalidFunction of string

let extract_metadata (fn : RuntimeT.user_fn) :
    string * parameter list * tipe_ * string =
  let name =
    match fn.metadata.name with
    | Filled (_, name) ->
        name
    | _ ->
        raise (InvalidFunction "Missing function name")
  in
  let parameters : parameter list =
    List.map fn.metadata.parameters ~f:(fun p ->
        match (p.name, p.tipe) with
        | Filled (_, name), Filled (_, tipe) ->
            ({name; tipe; description = p.description} : parameter)
        | Filled (_, name), _ ->
            raise (InvalidFunction "Missing tipe for function parameter")
        | _, Filled (_, tipe) ->
            raise (InvalidFunction "Missing name for function parameter")
        | _, _ ->
            raise
              (InvalidFunction "Invalid name and tipe for function parameter"))
  in
  let return_type =
    match fn.metadata.return_type with
    | Filled (_, return_type) ->
        return_type
    | _ ->
        raise (InvalidFunction "Invalid return type")
  in
  (name, parameters, return_type, fn.metadata.description)


let parse_fnname (name : string) : string * string * string * string * int =
  let regex_of str = Re2.create_exn ("^" ^ str ^ "$") in
  let user_pattern = "[a-z][a-z0-9_]+" in
  let package_pattern = "[a-z][a-z0-9A-Z]+" in
  let module_pattern = "[A-Z][a-z0-9A-Z_]+" in
  let fnname_pattern = "[a-z][a-z0-9A-Z_]+" in
  let version_pattern = "[0-9]+" in
  let toplevel_regex =
    (* regexes same as libbackend/account.ml and client/Autocomplete.ml *)
    regex_of "(.*)/(.*)/(.*)::(.*)_v(.*)"
  in
  let results =
    name
    |> Re2.get_matches ~max:1 toplevel_regex
    |> Or_error.ok
    |> Option.bind ~f:List.hd
    |> Option.map ~f:Re2.without_trailing_none
    |> Option.map ~f:Re2.Match.get_all
  in
  let must_match (pattern : string) (value : string) (help_name : string) =
    if Re2.matches (regex_of pattern) value
    then value
    else
      raise
        (InvalidFunction
           ("Invalid function name: " ^ help_name ^ " should match " ^ pattern))
  in
  match results with
  | Some [|_; Some user; Some package; Some module_; Some fnname; Some version|]
    ->
      let user = must_match user_pattern user "user_or_org" in
      let package = must_match package_pattern package "package" in
      let module_ = must_match module_pattern module_ "module_" in
      let fnname = must_match fnname_pattern fnname "fnname" in
      let version = must_match version_pattern version "version" in
      (user, package, module_, fnname, Int.of_string version)
  | other ->
      raise
        (InvalidFunction
           "Invalid function name, missing part of the name. It should match {user_or_org}/{package}/{module}::{fnname}_v{number}")


type expr_with_id =
  { tlid : tlid
  ; expr : RuntimeT.expr }
[@@deriving bin_io, show]

let expr_to_string (tlid : tlid) (expr : RuntimeT.expr) : string =
  {expr; tlid}
  |> Core_extended.Bin_io_utils.to_line bin_expr_with_id
  |> Bigstring.to_string


let string_to_expr (str : string) : expr_with_id =
  Core_extended.Bin_io_utils.of_line str bin_expr_with_id


let function_name (fn : fn) : string =
  fn.module_ ^ "::" ^ fn.fnname ^ "_v" ^ string_of_int fn.version


let add_function (fn : fn) : unit =
  let user =
    fn.user
    |> Account.id_of_username
    |> Option.value_exn ~message:"Invalid package owner"
  in
  let author =
    Account.id_of_username fn.author
    |> Option.value_exn ~message:"Invalid author"
  in
  Db.run
    ~name:"add_package_management_function"
    "INSERT INTO packages_v0 (tlid, user_id, package, module, fnname, version,
                              description, body, return_type, parameters, author_id, deprecated)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9,
            $10::jsonb, $11, $12)"
    ~subject:(function_name fn)
    ~params:
      [ Db.Int63 fn.tlid
      ; Db.Uuid user
      ; Db.String fn.package
      ; Db.String fn.module_
      ; Db.String fn.fnname
      ; Db.Int fn.version
      ; Db.String fn.description
      ; Db.Binary (expr_to_string fn.tlid fn.body)
      ; Db.String (Dval.tipe_to_string fn.return_type)
      ; Db.String
          (fn.parameters |> parameters_to_yojson |> Yojson.Safe.to_string)
      ; Db.Uuid author
      ; Db.Bool fn.deprecated ]
    ~result:TextResult ;
  (* subquery of a select *)
  (* TODO select 1 where version < version, and if false then there's nothing *)
  ()


let save (author : string) (fn : RuntimeT.user_fn) : (unit, string) Result.t =
  (* First let's be very sure we have a correct function *)
  try
    let name, parameters, return_type, description = extract_metadata fn in
    let user, package, module_, fnname, version = parse_fnname name in
    (* Binary values can only be fetched on their own, so we create an ID and
     * encode it within the binary-serialized value, so we know what function
     * it belongs to when fetching back later.  Uuids would be better but
     * don't serialize using bin_io. *)
    let tlid = Util.create_id () in
    add_function
      { user
      ; tlid
      ; package
      ; module_
      ; fnname
      ; version
      ; body = fn.ast
      ; return_type
      ; parameters
      ; description
      ; author
      ; deprecated = false } ;
    Ok ()
  with
  | InvalidFunction msg ->
      Error msg
  | Exception.DarkException {tipe = DarkStorage; short; _}
    when Tc.String.contains ~substring:"duplicate key" short ->
      Error "Function already exists"
  | e ->
      (* Error "Problem saving function" *)
      Error ("Unknown error: " ^ Exception.to_string e)


(* ------------------ *)
(* Fetching functions *)
(* ------------------ *)

let all_functions () : fn list =
  let metadata =
    Db.fetch
      ~name:"package_management_all_functions"
      ~params:[]
      "SELECT tlid, A.username, package, module, fnname, version,
              description, return_type, parameters, O.username, deprecated
       FROM packages_v0, accounts A, accounts O
       WHERE packages_v0.user_id = A.id
         AND packages_v0.author_id = O.id"
      ~result:TextResult
    |> List.filter_map ~f:(function
           | [ tlid
             ; user
             ; package
             ; module_
             ; fnname
             ; version
             ; description
             ; return_type
             ; parameters
             ; author
             ; deprecated ] ->
             ( try
                 Some
                   ( { user
                     ; package
                     ; module_
                     ; fnname
                     ; version = Int.of_string version
                     ; body =
                         (* placeholder, it gets overwritten in the next query *)
                         Blank (Int63.of_int 5)
                     ; return_type = Dval.tipe_of_string return_type
                     ; parameters =
                         parameters
                         |> Yojson.Safe.from_string
                         |> parameters_of_yojson
                         |> Result.ok_or_failwith
                     ; description
                     ; author
                     ; deprecated = deprecated = "t"
                     ; tlid = id_of_string tlid }
                     : fn )
               with _ -> None )
           | _ ->
               Exception.internal "Bad format for package_manager.all_functions")
  in
  let bodies =
    Db.fetch
      ~name:"package_management_function_data"
      ~params:[]
      "SELECT body FROM packages_v0"
      ~result:BinaryResult
    |> List.filter_map ~f:(function
           | [body] ->
             ( try
                 let body = string_to_expr body in
                 Some (string_of_id body.tlid, body.expr)
               with _ -> None )
           | _ ->
               Exception.internal "Bad format for package_manager.all_functions")
    |> Tc.StrDict.from_list
  in
  let functions =
    metadata
    |> List.map ~f:(fun fn ->
           let key = string_of_id fn.tlid in
           let body =
             bodies
             |> Tc.StrDict.get ~key
             |> Option.value_exn ~message:("No body found for " ^ key)
           in
           {fn with body})
  in
  functions


let runtime_fn_of_package_fn (fn : fn) : RuntimeT.fn =
  ( { prefix_names = [to_name fn]
    ; infix_names = []
    ; parameters = fn.parameters |> List.map ~f:runtime_param_of_parameter
    ; return_type = fn.return_type
    ; description = fn.description
    ; func = PackageFunction fn.body
    ; preview_execution_safe = false
    ; deprecated = fn.deprecated }
    : RuntimeT.fn )


let to_get_packages_rpc_result packages : string =
  packages
  |> get_packages_rpc_result_to_yojson
  |> Yojson.Safe.to_string ~std:true


let packages : fn list ref = ref []

(* This can't happen until migrations have run, or we might not have a
 * packages_v0 table *)
let init () = packages := all_functions ()
