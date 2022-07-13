open Prelude
open Tester
open FluidToken

let run = () => {
  describe("analysisID of token", () => {
    test("returns id of varBind if token is TLetVarName", () => {
      let leftLetToken = TLetVarName(ID.fromInt(1), ID.fromInt(2), "a", None)
      expect(analysisID(leftLetToken)) |> toEqual(ID.fromInt(2))
    })
    test("returns id of record field name if token is TRecordFieldname ", () => {
      let leftLetToken = TRecordFieldname({
        recordID: ID.fromInt(1),
        exprID: ID.fromInt(2),
        index: 1,
        fieldName: "name",
        parentBlockID: None,
      })

      expect(analysisID(leftLetToken)) |> toEqual(ID.fromInt(2))
    })
    test("return ids of", () => {
      let lambdaVar = TLambdaVar(ID.fromInt(1), ID.fromInt(2), 1, "var", None)

      expect(analysisID(lambdaVar)) |> toEqual(ID.fromInt(2))
    })
    ()
  })
  ()
}
