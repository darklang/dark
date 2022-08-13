// Basic types

module Tea_result = {
  include Tea_result
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

// Extend Tea functions
module Tea = {
  module Result = Tea_result
  module Cmd = Tea_cmd
  module Sub = Tea_sub
  module App = Tea_app
  module Debug = Tea_debug
  module Html = Tea_html_extended
  module Html2 = Tea_html2
  module Svg = Tea_svg
  module Task = Tea_task
  module Program = Tea_program
  module Time = Tea_time_extended
  module Json = Tea_json
  module Navigation = Tea_navigation
  module Random = Tea_random
  module AnimationFrame = Tea_animationframe
  module Mouse = Tea_mouse
  module Ex = Tea_ex
  module Http = {
    include Tea_http
    let pp_error = (
      _valueFormatter: (Format.formatter, 'v) => unit,
      fmt: Format.formatter,
      _value: error<'v>,
    ) => {
      Format.pp_print_string(fmt, "Tera.Http.error")
      // valueFormatter(fmt, value)
    }
  }
}

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
