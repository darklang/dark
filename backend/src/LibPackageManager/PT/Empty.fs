module LibPackageManager.PT.Empty

module PT = LibExecution.ProgramTypes

let empty : PT.PackageManager = InMemory.create []
