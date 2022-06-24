open Tester
open Prelude
open FluidTestData
module E = FluidExpression
module T = FluidToken

let run = () => {
  let tokensFor = (e: E.t): list<T.t> => FluidTokenizer.tokenize(e) |> List.map(~f=ti => ti.token)

  let hasTokenMatch = (~f: T.t => bool, e: E.t) => {
    let tokens = tokensFor(e)
    expect(List.any(tokens, ~f)) |> toEqual(true)
  }

  describe("toTokens' converts expressions to tokens", () => {
    test("field access keeps parentBlockID", () => {
      let parentID = gid()
      let expr = E.EList(parentID, list{EFieldAccess(gid(), EVariable(gid(), "obj"), "field")})

      let tokens = tokensFor(expr)
      expect(
        List.any(tokens, ~f=x =>
          switch x {
          | TVariable(_, _, Some(pbid)) if pbid == parentID => true
          | _ => false
          }
        ),
      ) |> toEqual(true)
    })
    test("empty object literal does not have parentBlockID", () =>
      emptyRecord |> hasTokenMatch(~f=x =>
        switch x {
        | TRecordOpen(_, None) | TRecordClose(_, None) => true
        | _ => false
        }
      )
    )
    test("object literal keeps parentBlockID in fields", () => {
      let parentBlockID = gid()
      E.ERecord(parentBlockID, list{recordRow1}) |> hasTokenMatch(~f=x =>
        switch x {
        | TRecordFieldname(d) if d.parentBlockID == Some(parentBlockID) => true
        | _ => false
        }
      )
    })
    test("empty list literal does not have parentBlockID", () =>
      emptyList |> hasTokenMatch(~f=x =>
        switch x {
        | TListOpen(_, None) | TListClose(_, None) => true
        | _ => false
        }
      )
    )
    test("list literal keeps parentBlockID in the items", () => {
      let parentBlockID = gid()
      E.EList(parentBlockID, list{fiftySix, seventyEight}) |> hasTokenMatch(~f=x =>
        switch x {
        | TInteger(_, _, pid) if pid == Some(parentBlockID) => true
        | _ => false
        }
      )
    })
  })
  ()
}
