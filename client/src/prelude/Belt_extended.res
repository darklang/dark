module Belt = {
  include (Belt: module type of Belt with module Map := Belt.Map and module Result := Belt.Result)

  module Result = {
    include Belt.Result
  }

  module Map = {
    include (Belt.Map: module type of Belt.Map with module String := Belt.Map.String)

    module String = {
      include Belt.Map.String
    }
  }
}
