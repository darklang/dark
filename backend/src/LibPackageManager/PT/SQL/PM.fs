module LibPackageManager.PT.SQL.PM

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open LibPackageManager.PT.SQL

module PT = LibExecution.ProgramTypes


let pt : PT.PackageManager =
  { findType = Types.find
    findValue = Values.find
    findFn = Fns.find

    getType = Types.get
    getFn = Fns.get
    getValue = Values.get

    getTypeLocation = Types.getLocation
    getValueLocation = Values.getLocation
    getFnLocation = Fns.getLocation

    search = Search.search

    applyOps = OpPlayback.applyOps

    init = uply { return () } }
