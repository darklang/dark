open Prelude
open Tester
module Ds = DarkStorage

let d2 = {
  deployHash: "abc123",
  url: "",
  lastUpdate: Js.Date.fromString("2019-01-02"),
  status: Deployed,
}

let d1 = {
  deployHash: "def456",
  url: "",
  lastUpdate: Js.Date.fromString("2019-01-01"),
  status: Deployed,
}

let d3 = {
  deployHash: "xyz789",
  url: "",
  lastUpdate: Js.Date.fromString("2019-01-03"),
  status: Deploying,
}

let originalList: list<staticDeploy> = list{d2, d1, d3}

let run = () => {
  describe("appendDeploy", () => {
    test("sort by lastUpdate timestamp", () => {
      let sortedList = list{d3, d2, d1}
      expect(Ds.appendDeploy(list{}, originalList)) |> toEqual(sortedList)
    })
    test("de-dup by deployHash and choose the one with most recent lastUpdate", () => {
      let newDeploy = {
        deployHash: "xyz789",
        url: "",
        lastUpdate: Js.Date.fromString("2019-01-04"),
        status: Deploying,
      }

      let mergedList = list{newDeploy, d2, d1}
      expect(Ds.appendDeploy(list{newDeploy}, originalList)) |> toEqual(mergedList)
    })
    ()
  })
  ()
}
