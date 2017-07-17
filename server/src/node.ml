
type loc = { x: int; y: int}
type id = int
type param = string
type dval = Runtime.dval

class virtual node id loc =
  object (self)
    val id : id = id
    val mutable loc : loc = loc
    method virtual name : string
    method virtual tipe : string
    method virtual execute : dval list -> dval
    method id = id
    method is_page = false
    method is_datasink = false
    method is_datasource = false
    method update_loc _loc =
      loc <- _loc
    method to_frontend : Yojson.Basic.json =
      `Assoc (List.append
                [ ("name", `String self#name)
                ; ("id", `Int id)
                ; ("type", `String self#tipe)
                ; ("x", `Int loc.x)
                ; ("y", `Int loc.y)
                ]
                self#extra_fields)
    method extra_fields = []
  end;;

class value strrep id loc =
  object (self)
    inherit node id loc
    val expr : dval = Runtime.parse strrep
    method name : string = strrep
    method tipe = "value"
    method execute (args : dval list) : dval = expr
    method extra_fields = [("value", `String strrep)]
  end;;

class func name id loc =
  object (self)
    inherit node id loc
    (* Throw an exception if it doesn't exist *)
    val name : string = let _ = Lib.get_fn name in name
    method name = name
    method execute (args : dval list) =
      Runtime.exe (Lib.get_fn name) args
    method is_page = name == "Page_page"
    method tipe = if (Core.String.is_substring "page" name)
      then name
      else "function"
    method extra_fields =
      [("parameters",
        `List (List.map
                 (fun s -> `String s)
                 (Lib.get_fn name).parameters))]
  end;;

class datastore table id loc =
  object (self)
    inherit node id loc
    val table : string = table
    method execute (args : dval list) : dval = DStr "todo"
    method name = "DS-" ^ table
    method tipe = "datastore"
  end;;
