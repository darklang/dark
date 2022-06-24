open Prelude
open Tester
open FluidToken

let run = () => {
  describe("analysisID of token", () => {
    test("returns id of varBind if token is TLetVarName", () => {
      let leftLetToken = TLetVarName(ID("1"), ID("2"), "a", None)
      expect(analysisID(leftLetToken) |> ID.toString) |> toEqual("2")
    })
    test("returns id of record field name if token is TRecordFieldname ", () => {
      let leftLetToken = TRecordFieldname({
        recordID: ID("1"),
        exprID: ID("2"),
        index: 1,
        fieldName: "name",
        parentBlockID: None,
      })

      expect(analysisID(leftLetToken) |> ID.toString) |> toEqual("2")
    })
    test("return ids of", () => {
      let lambdaVar = TLambdaVar(ID.fromString("1"), ID.fromString("2"), 1, "var", None)

      expect(analysisID(lambdaVar) |> ID.toString) |> toEqual("2")
    })
    ()
  })
  ()
}
