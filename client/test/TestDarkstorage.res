// open Prelude
open Tester

module Ds = DarkStorage
module D = StaticAssets.Deploy

let d2: D.t = {
  deployHash: "abc123",
  url: "",
  lastUpdate: Js.Date.fromString("2019-01-02"),
  status: Deployed,
}

let d1: D.t = {
  deployHash: "def456",
  url: "",
  lastUpdate: Js.Date.fromString("2019-01-01"),
  status: Deployed,
}

let d3: D.t = {
  deployHash: "xyz789",
  url: "",
  lastUpdate: Js.Date.fromString("2019-01-03"),
  status: Deploying,
}

let originalList: list<StaticAssets.Deploy.t> = list{d2, d1, d3}

let run = () => {
  describe("appendDeploy", () => {
    test("sort by lastUpdate timestamp", () => {
      let sortedList = list{d3, d2, d1}
      expect(Ds.appendDeploy(list{}, originalList)) |> toEqual(sortedList)
    })
    test("de-dup by deployHash and choose the one with most recent lastUpdate", () => {
      let newDeploy: D.t = {
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
