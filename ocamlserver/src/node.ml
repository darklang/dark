type loc = { x: int; y: int}
type id = int
type param = string

class virtual node id loc =
  object (self)
    val id : id = id
    val loc : loc = loc
    method virtual name : string
    method virtual idstr : string
    method virtual parameters : string list
    method virtual tipe : string
    method id = id
    method short_id = id mod 65536
    method is_page = false
    method is_datasink = false
    method is_datasource = false
    method to_frontend : Yojson.Basic.json =
      `Assoc [ ("name", `String self#name)
             ; ("parameters",
                `List (List.map (fun s -> `String s) self#parameters))
             ; ("id", `String self#idstr)
             ; ("type", `String self#tipe)
             ; ("x", `Int loc.x)
             ; ("y", `Int loc.y)
             ]
  end;;

class value expr id loc =
  object (self)
    inherit node id loc
    val expr : string = expr
    method name : string = expr
    method idstr : string = Printf.sprintf "VALUE-%x (%s)" id expr
    method tipe = "value"
    method parameters = []
  end;;

class func name id loc =
  object (self)
    inherit node id loc
    val name : string = name
    method name = name
    method idstr = Printf.sprintf "%s-%04X" name id
    method is_page = name == "Page_page"
    method tipe = if (Core.String.is_substring "page" name)
      then name
      else "function"
    method parameters = (Lib.get_fn name).parameters
  end;;

class datastore table id loc =
  object (self)
    inherit node id loc
    val table : string = table
    method name = "DS-" ^ table
    method idstr = table
    method tipe = "datastore"
    method parameters = []
  end;;
