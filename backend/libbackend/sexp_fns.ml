open Libexecution.Lib
open Libexecution.Types.RuntimeT
module RTT = Libexecution.Types.RuntimeT
open Core_kernel

(* Really this should possibly be in libexecution; some of these fns may run
 * client-side. But since we're pulling from a file, this is it for now *)
let sexp_fns : fn list =
  let files =
    (*File.lsdir ~root:Config.Testdata "/"*)
    Core.Sys.ls_dir (Config.dir Config.Testdata ^ "/")
    |> Tablecloth.List.filter ~f:(fun file ->
           String.is_prefix file ~prefix:"sexp-")
  in
  files
  |> List.map ~f:(fun file ->
         let str = File.readfile ~root:Config.Testdata file in
         let sexp = str |> Sexp.of_string in
         let name_prefix =
           file
           |> String.split_on_chars ~on:['-'; '.']
           |> List.filter ~f:(fun s -> s <> "sexp" && s <> "json")
           |> List.map ~f:Tablecloth.String.capitalize
           |> String.concat ~sep:""
         in
         let sexps =
           match sexp with
           | Sexp.List sexps ->
               sexps
           | Sexp.Atom _ ->
               Libexecution.Exception.internal ("BAD FILE: " ^ file)
         in
         sexps
         |> List.map ~f:(fun sexp ->
                match sexp with
                | Sexp.List
                    [ Sexp.Atom name
                    ; Sexp.Atom description
                    ; Sexp.List params
                    ; Sexp.Atom return_type
                    ; ast ] ->
                    { prefix_names = [name_prefix ^ "::" ^ name]
                    ; infix_names = []
                    ; parameters =
                        params
                        |> List.map ~f:(fun param ->
                               let name, tipe =
                                 match param with
                                 | Sexp.List [Sexp.Atom name; Sexp.Atom tipe] ->
                                     ( name
                                     , tipe |> Libexecution.Types.tipe_of_str )
                                 | _ ->
                                     Libexecution.Exception.internal
                                       "BAD FORMAT"
                               in
                               ( { name
                                 ; tipe
                                 ; block_args = []
                                 ; optional = false
                                 ; description = "" }
                                 : RTT.param ))
                    ; return_type = Libexecution.Types.tipe_of_str return_type
                    ; description
                    ; func =
                        UserCreated
                          (* TODO do we actually want UserCreated here, and not
                           * InProcess/API? *)
                          ( Libexecution.Types.id_of_int 0
                            (* TODO use a better id here ... *)
                          , Expr_dsl.ast_for (Sexp.to_string ast) )
                    ; preview_execution_safe =
                        false
                        (* TODO preview exn safe could
                                                     be true? *)
                    ; deprecated = false (* TODO deprecated could be true *) }
                | _ ->
                    Libexecution.Exception.internal "Bad format"))
  |> Tablecloth.List.flatten
  |> fun fns ->
  fns
  |> List.iter ~f:(fun fn ->
         Caml.print_endline ("FN: " ^ List.hd_exn fn.prefix_names)) ;
  fns


let shortfn_of_fn (f : fn) : shortfn =
  { pns = f.prefix_names
  ; ins = f.infix_names
  ; r = f.return_type
  ; p = f.parameters
  ; d = f.description
  ; f = f.func
  ; ps = f.preview_execution_safe
  ; dep = f.deprecated }


let fns = sexp_fns |> List.map ~f:shortfn_of_fn
