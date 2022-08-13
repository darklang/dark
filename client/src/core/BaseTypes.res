/// Basic types
/// Does this *extend* basic types, or what? `pp` is a reall unclear fn name.

module Belt = {
  // what is this? Could it somehow be more clear?
  include (Belt: module type of Belt with module Map := Belt.Map and module Result := Belt.Result)

  module Result = {
    include Belt.Result

    // what does this do?
    // When I remove it, it errors in RuntimeTypes.res, but I can't figure out why/where.
    // something related to all of these
    let pp = (
      okValueFormatter: (Format.formatter, 'okValue) => unit,
      errValueFormatter: (Format.formatter, 'errValue) => unit,
      fmt: Format.formatter,
      value: t<'okValue, 'errValue>,
    ) => {
      switch value {
      | Ok(value) =>
        Format.pp_print_string(fmt, "Ok")
        okValueFormatter(fmt, value)
      | Error(value) =>
        Format.pp_print_string(fmt, "Error")
        errValueFormatter(fmt, value)
      }
    }
  }

  module Map = {
    include (Belt.Map: module type of Belt.Map with module String := Belt.Map.String)

    module String = {
      include Belt.Map.String

      let pp = (
        valueFormatter: (Format.formatter, 'value) => unit,
        fmt: Format.formatter,
        map: t<'value>,
      ) => {
        Format.pp_print_string(fmt, "{ ")
        Belt.Map.String.forEach(map, (key, value) => {
          Format.pp_print_string(fmt, key)
          Format.pp_print_string(fmt, ": ")
          valueFormatter(fmt, value)
          Format.pp_print_string(fmt, ",  ")
        })
        Format.pp_print_string(fmt, "}")
        ()
      }
    }
  }
}
