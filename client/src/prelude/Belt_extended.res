module Belt = {
  include (Belt: module type of Belt with module Map := Belt.Map and module Result := Belt.Result)

  module Result = {
    include Belt.Result

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
