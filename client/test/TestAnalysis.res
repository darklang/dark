open Prelude
open Tester
open Analysis
module B = BlankOr

let run = () => {
  describe("requestAnalysis", () =>
    test("on tlid not found", () => {
      let m = {...Defaults.defaultModel, deletedUserFunctions: TLID.Dict.empty}

      expect(requestAnalysis(m, TLID(123l), "abc")) |> toEqual(Cmd.none)
    })
  )
  ()
}
