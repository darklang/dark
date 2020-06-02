open Prelude

let pmParamsToUserFnParams (p : packageFnParameter) : userFunctionParameter =
  { ufpName = BlankOr.newF p.name
  ; ufpTipe = BlankOr.newF p.tipe
  ; ufpBlock_args = []
  ; ufpOptional = false
  ; ufpDescription = p.description }


let paramData (pfp : packageFnParameter) : blankOrData list =
  let paramName = BlankOr.newF pfp.name in
  let paramTipe = BlankOr.newF pfp.tipe in
  [PParamName paramName; PParamTipe paramTipe]


let allParamData (pmf : packageFn) : blankOrData list =
  List.concat (List.map ~f:paramData pmf.parameters)


let blankOrData (pmf : packageFn) : blankOrData list =
  let fnname = BlankOr.newF pmf.fnname in
  [PFnName fnname] @ allParamData pmf


let extendedName (pkgFn : packageFn) : string =
  Printf.sprintf
    "%s/%s/%s::%s_v%d"
    pkgFn.user
    pkgFn.package
    pkgFn.module_
    pkgFn.fnname
    pkgFn.version


let fn_of_packageFn (pkgFn : packageFn) : function_ =
  let paramOfPkgFnParam (pkgFnParam : packageFnParameter) : parameter =
    { paramName = pkgFnParam.name
    ; paramTipe = pkgFnParam.tipe
    ; paramDescription = pkgFnParam.description
    ; paramBlock_args =
        []
        (* We don't currently support block args in package
                             fns *)
    ; paramOptional = false }
  in
  { fnName = pkgFn |> extendedName
  ; fnParameters = pkgFn.parameters |> List.map ~f:paramOfPkgFnParam
  ; fnDescription = pkgFn.description
  ; fnReturnTipe = pkgFn.return_type
  ; fnPreviewSafety = Unsafe
  ; fnDeprecated = pkgFn.deprecated
  ; fnInfix = false
  ; fnOrigin = PackageManager }
