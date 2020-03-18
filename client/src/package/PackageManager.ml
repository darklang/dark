open Prelude

let loadPackages (packages : packageFns) (loadedPackages : packageFn list) :
    packageFns =
  loadedPackages
  |> List.map ~f:(fun pf -> (pf.pfTLID, pf))
  |> TLIDDict.fromList
  |> TLIDDict.mergeRight packages


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
  let to_name (fn : packageFn) : string =
    Printf.sprintf
      "%s/%s/%s::%s_v%d"
      fn.user
      fn.package
      fn.module_
      fn.fnname
      fn.version
  in
  { fnName = pkgFn |> to_name
  ; fnParameters = pkgFn.parameters |> List.map ~f:paramOfPkgFnParam
  ; fnDescription = pkgFn.description
  ; fnReturnTipe = pkgFn.return_type
  ; fnPreviewSafety = Unsafe
  ; fnDeprecated = pkgFn.deprecated
  ; fnInfix = false }
