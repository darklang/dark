/// Sync, from a store with no relay: identity, and what every sync verb does when
/// there is nowhere to sync to.
///
/// The two-instance behaviour (push, pull, promotion, conflicts) needs two stores
/// and a relay between them, which is `scripts/testing/_gates-sync`, not this suite.
/// What belongs HERE is the half that has no network in it and is what most people
/// meet first: a fresh install typing `dark push` before `dark connect`, and typing
/// the url wrong when they do.
module Tests.CliSyncSurface

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

module RT = LibExecution.RuntimeTypes

open Tests.CliTestHarness
open Tests.CliDsl


let identityIsStableAndSharedBetweenBothNames =
  cliTest "whoami and identity name the same instance" (fun state ->
    task {
      let! whoami = runCli state [ "whoami" ]
      let! identity = runCli state [ "identity" ]
      Expect.stringContains
        whoami
        "inst-"
        $"whoami names an instance, got: {whoami}"
      Expect.equal identity whoami $"identity is the same answer, got: {identity}"

      let! again = runCli state [ "whoami" ]
      Expect.equal
        again
        whoami
        $"and asking twice does not mint a new one, got: {again}"
    })

/// Every verb, one test: what they have in common is the thing worth pinning. A
/// fresh install has no relay, and each of these has to say so and say what to do
/// about it, rather than erroring about a url it never had.
let everySyncVerbSaysThereIsNoRelayYet =
  cliTest "the sync verbs say there's no relay, and what to do" (fun state ->
    task {
      do!
        showsAll
          state
          [ "sync"; "status" ]
          [ "no relay"; "dark connect" ]
          "sync status says there's no relay yet"
      do!
        showsAll
          state
          [ "push" ]
          [ "no relay"; "dark connect" ]
          "and so does bare push"
      do! showsAll state [ "pull" ] [ "no relay"; "dark connect" ] "and bare pull"
    })

/// `dark sync push` is the typo that gets typed: `push` is a verb everywhere else, and `sync`
/// reads its lone argument as a url. Carried to the http client it comes back as "push failed:
/// bad url", which reads like a broken relay rather than a mistyped command.
let aVerbInTheUrlPositionIsRefusedThere =
  cliTest "a word in the url position is refused as a url" (fun state ->
    task {
      do!
        refuses
          state
          [ "sync"; "push" ]
          "not a relay url"
          "bad url"
          "sync says what's wrong with the argument"
      do!
        refuses
          state
          [ "push"; "somewhere" ]
          "not a relay url"
          "bad url"
          "push does too"
      do!
        refuses state [ "pull"; "somewhere" ] "not a relay url" "bad url" "and pull"
      do! exits state [ "sync"; "push" ] 1L "and it's a failed command"
    })

/// Regression: `dark connect notaurl` STORED `notaurl` as the relay, then failed the
/// push against it with "bad url", then told you `dark push` would retry -- a retry
/// that could never work. An unreachable url is kept on purpose (connecting offline
/// is legitimate); one with no scheme is not a url at all.
let connectRefusesSomethingThatIsNotAUrl =
  cliTest "connect refuses a string that could never be a relay" (fun state ->
    task {
      do!
        refuses
          state
          [ "connect"; "notaurl" ]
          "not a relay url"
          "relay remembered"
          "connect says why it won't take it"
      do! exits state [ "connect"; "notaurl" ] 1L "and it's a failed command"
      // And it did not store it: the sync verbs still report a fresh install.
      do! shows state [ "sync"; "status" ] "no relay" "nothing was remembered"
    })

/// `pull --all` rewinds this relay's cursor before pulling, so it has to be a flag the verb knows
/// rather than something it takes for a url. With no relay there is nothing to rewind, and the
/// answer is the same "no relay" every other sync verb gives.
let pullKnowsItsOwnFlags =
  cliTest "pull takes --all, and refuses a flag it does not know" (fun state ->
    task {
      do!
        refuses
          state
          [ "pull"; "--nope" ]
          "unknown flag"
          "re-pulling"
          "an unknown flag is named, not read as a url"
      do! exits state [ "pull"; "--nope" ] 1L "and it is a failed command"

      do!
        shows
          state
          [ "pull"; "--all" ]
          "no relay"
          "--all is understood, and there is still nowhere to pull from"
    })

let syncHelpNamesItsVerbs =
  cliTest "sync help names the verbs it has" (fun state ->
    task {
      do!
        showsAll
          state
          [ "sync"; "help" ]
          [ "setup"; "status"; "export"; "import" ]
          "the help lists the surface"
      do!
        shows
          state
          [ "sync"; "nosuchverb" ]
          "nosuchverb"
          "and an unknown verb names itself back"
    })

let exportSeedExplainsItself =
  cliTest "export-seed explains what it wants" (fun state ->
    task {
      do!
        showsAll
          state
          [ "export-seed" ]
          [ "Usage"; "seed" ]
          "with no path it says what it needs"
    })


/// An identity travels as a query parameter and comes back in the relay's owner listing, one per
/// line. A space makes that listing unparseable, so every peer's automatic branch sync skips you; an
/// `&` reads as a second parameter, so ops land under two owners. Both fail silently, which is why
/// the refusal belongs at the point the name is chosen.
let anIdentityIsRefusedIfItCannotTravel =
  cliTest "an identity that would break sync is refused when it is set" (fun state ->
    task {
      do!
        refuses
          state
          [ "identity"; "has a space" ]
          "can't contain"
          "is now"
          "a space is refused"
      do!
        refuses
          state
          [ "identity"; "amp&sand" ]
          "can't contain"
          "is now"
          "an ampersand is refused"
      do! exits state [ "identity"; "has a space" ] 1L "and it is a failed command"

      do!
        shows
          state
          [ "identity"; "alice-laptop_2.0" ]
          "alice-laptop_2.0"
          "an ordinary name is taken"
      // Leave the store as it was found: identity is per-instance config every later test reads.
      do! run state [ "identity"; "inst-test-restored" ]
    })

let tests : List<Test> =
  [ identityIsStableAndSharedBetweenBothNames
    everySyncVerbSaysThereIsNoRelayYet
    aVerbInTheUrlPositionIsRefusedThere
    connectRefusesSomethingThatIsNotAUrl
    pullKnowsItsOwnFlags
    syncHelpNamesItsVerbs
    exportSeedExplainsItself
    anIdentityIsRefusedIfItCannotTravel ]
