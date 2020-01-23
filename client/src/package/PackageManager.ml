open Prelude

let loadPackages (packages : packages) (loadedPackages : packageFn list) :
    packages =
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
  { fnName =
      Printf.sprintf
        "%s/%s/%s::%s_v%s"
        pkgFn.user
        pkgFn.package
        pkgFn.module_
        pkgFn.fnname
        (string_of_int pkgFn.version)
  ; fnParameters = pkgFn.parameters |> List.map ~f:paramOfPkgFnParam
  ; fnDescription = pkgFn.description
  ; fnReturnTipe = pkgFn.return_type
  ; fnPreviewExecutionSafe = false
  ; fnDeprecated = pkgFn.deprecated
  ; fnInfix = false }
