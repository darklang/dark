open Tester
open Types
open Analysis
module B = Blank

let run () =
  describe "requestAnalysis" (fun () ->
      test "on tlid not found" (fun () ->
          let m =
            {Defaults.defaultModel with deletedUserFunctions = TLIDDict.empty}
          in
          expect (requestAnalysis m (TLID "123") "abc") |> toEqual Cmd.none )
  ) ;
  ()
