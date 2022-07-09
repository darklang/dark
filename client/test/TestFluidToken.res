open Prelude
open Tester
open FluidToken

let run = () => {
  describe("analysisID of token", () => {
    test("returns id of varBind if token is TLetVarName", () => {
      let leftLetToken = TLetVarName(ID(1l), ID(2l), "a", None)
      expect(analysisID(leftLetToken)) |> toEqual(ID.ID(2l))
    })
    test("returns id of record field name if token is TRecordFieldname ", () => {
      let leftLetToken = TRecordFieldname({
        recordID: ID(1l),
        exprID: ID(2l),
        index: 1,
        fieldName: "name",
        parentBlockID: None,
      })

      expect(analysisID(leftLetToken)) |> toEqual(ID.ID(2l))
    })
    test("return ids of", () => {
      let lambdaVar = TLambdaVar(ID.ID(1l), ID.ID(2l), 1, "var", None)

      expect(analysisID(lambdaVar)) |> toEqual(ID.ID(2l))
    })
    ()
  })
  ()
}
