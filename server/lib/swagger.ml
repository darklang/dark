open Core

(* Not taken from  *)
(* https://github.com/natebrennand/swagger/blob/master/schema.mli *)
(* but maybe in the future? *)

type parameter = { dataType: string
                 ; paramType: string
                 ; required: bool
                 ; name: string
                 ; description: string
                 } [@@deriving yojson]

type operation = { httpMethod: string
                 ; nickname: string
                 ; parameters: parameter list
                 ; responseClass: string
                 ; summary: string option
                 } [@@deriving yojson]

type api = { operations: operation list
           ; path: string
           ; description: string
           } [@@deriving yojson]

type schema = { basePath: string
              ; apis: api list
              ; swaggerVersion: string
              ; apiVersion: string
              ; description: string
              } [@@deriving yojson]

let parse (filename: string) : schema =
  match
    filename
    |> Util.readfile2
    |> Yojson.Safe.from_string
    |> schema_of_yojson
  with
  | Error loc -> Exception.internal ("Error parsing Swagger schema: " ^ loc)
  | Ok schema -> schema
