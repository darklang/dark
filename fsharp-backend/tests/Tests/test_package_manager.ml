let t_parsefnname () =
  let account, package, module_, fnname, version =
    Package_manager.parse_fnname "dark/stdlib/Twitter::sendText_v0"
  in
  AT.check AT.string "account" account "dark" ;
  AT.check AT.string "package" package "stdlib" ;
  AT.check AT.string "module" module_ "Twitter" ;
  AT.check AT.string "fnname" fnname "sendText" ;
  AT.check AT.int "version" version 0 ;
  let account, package, module_, fnname, version =
    Package_manager.parse_fnname "paul56/random/Rand56om::string20_v57"
  in
  AT.check AT.string "account" account "paul56" ;
  AT.check AT.string "package" package "random" ;
  AT.check AT.string "module" module_ "Rand56om" ;
  AT.check AT.string "fnname" fnname "string20" ;
  AT.check AT.int "version" version 57 ;
  ()
