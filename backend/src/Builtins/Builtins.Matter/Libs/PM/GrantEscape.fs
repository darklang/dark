/// The scripted-setup escape from "a person at a terminal": `DARK_GRANTS_WITHOUT_A_PERSON=1`,
/// set by the person in the environment of the command they start, never by a prompt or a
/// setting. Read once, when the process starts (`Cli.fs` forces it before any Dark code runs),
/// so code that sets the variable during a run does not turn it on for that run.
///
/// Its own module so the host-boundary scan can allow this one environment read and nothing
/// else in the policy builtins.
module Builtins.Matter.Libs.PM.GrantEscape

let grantsWithoutAPerson : Lazy<bool> =
  lazy
    (System.Environment.GetEnvironmentVariable "DARK_GRANTS_WITHOUT_A_PERSON" = "1")
