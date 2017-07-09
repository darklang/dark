type loc = { x: int; y: int}
type id = int

class virtual node id loc =
  object (self)
    val id : id = id
    val loc : loc = loc
    method virtual name : string
    method virtual id : string
    method short_id = id mod 65536
    method is_page = false
    method is_datasink = false
    method is_datasource = false
  end;;

class value expr id loc =
  object (self)
    inherit node id loc
    val expr : string = expr
    method name : string = expr
    method id : string = Printf.sprintf "VALUE-%x (%s)" id expr
  end;;

class func name id loc =
  object (self)
    inherit node id loc
    val name : string = name
    method name = name
    method id = Printf.sprintf "%s-%04X" name id
    method is_page = name == "Page_page"
  end;;

class datastore table id loc =
  object (self)
    inherit node id loc
    val table : string = table
    method name = "DS-" ^ table
    method id = table
  end;;
