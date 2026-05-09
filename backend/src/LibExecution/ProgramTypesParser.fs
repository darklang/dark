/// Vestigial. Used to host PT.Handler accessors; with Handler gone,
/// nothing left here. Kept as an empty module so ProjectReferences
/// don't fight the rename in this PR — delete the file in a follow-up.
module LibExecution.ProgramTypesParser

open Prelude

module PT = ProgramTypes
