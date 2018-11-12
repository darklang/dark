open Core_kernel

open Lib
open Types.RuntimeT

let fns : Lib.shortfn list = [
  { pns = ["DarkInternal::checkAccess"]
  ; ins = []
  ; p = []
  ; r = TNull
  ; d = "TODO"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DarkInternal::checkAllCanvases"]
  ; ins = []
  ; p = []
  ; r = TNull
  ; d = "TODO"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = true
  }
  ;

  { pns = ["DarkInternal::migrateAllCanvases"]
  ; ins = []
  ; p = []
  ; r = TNull
  ; d = "TODO"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DarkInternal::cleanupOldTraces"]
  ; ins = []
  ; p = []
  ; r = TNull
  ; d = "TODO"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DarkInternal::checkCanvas"]
  ; ins = []
  ; p = [par "host" TStr]
  ; r = TBool
  ; d = "TODO"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DarkInternal::getAllCanvases"]
  ; ins = []
  ; p = []
  ; r = TList
  ; d = "TODO"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  { pns = ["DarkInternal::canvasAsText"]
  ; ins = []
  ; p = [par "host" TStr]
  ; r = TStr
  ; d = "TODO"
  ; f = NotClientAvailable
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

  ]
