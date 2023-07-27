# Parser

Parses code as F#, and maps it into Dark code, programs, etc.


## Parsing process

The result of a parse is some ProgramTypes types. ProgramTypes are used to represent
how users write the code, but also have enough information to make RuntimeTypes
without further analysis. As such, when creating ProgramTypes we need to resolve
several ambiguities, including what a name refers to (eg what is MyFunc.myFn
actually) and the concrete type of an alias.

To get here, Dark has a 2-stage parsing process, where we first get all the code, and
secondly resolve the names/aliases.

Dark's parsing process:
1. FSharpToWrittenTypes
  - Parse the code using the F# parser
  - See https://fsharp.github.io/fsharp-compiler-docs for details on the F# AST
  - Traverse the F# AST and convert it to WrittenTypes
2. WrittenTypesToProgramTypes
  - Go through the AST, resolving names and aliases and creating ProgramTypes values.
    This will involve fetching the actual names and code in the case of packages and
    userProgram contents.
  - Names are resolved with a NameResolver
  - TODO: Aliases will be resolved
  - If errors are found, the errors are encoded in ProgramTypes, where they will
    error at runtime (it is a goal of Dark to be able to run incomplete and broken code
    to allow debugging/analysis).
  - We can add various options for how to statically report the presence of these
    errors, including warnings at parse time or program analysis.
3. ProgramTypes
  - Once we have ProgramTypes, the parsing is finished. ProgramTypes are the
    representation used by clients and editors. They're also used for the execution
    environment, where they are fetched as ProgramTypes and converted very simply to
    RuntimeTypes. They are stored as SerializedTypes, which is a 1-1 representation of ProgramTypes.