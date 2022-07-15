open Prelude
open Tester
open Introspect
module TL = Toplevel
module B = BlankOr

let run = () => {
  describe("Introspect", () => {
    let h1tlid = gtlid()
    let h1data = {
      ast: FluidAST.ofExpr(EBlank(gid())),
      spec: {
        space: B.newF("WORKER"),
        name: B.newF("processOrder"),
        modifier: B.new_(),
      },
      hTLID: h1tlid,
      pos: {x: 0, y: 0},
    }

    let h2tlid = gtlid()
    let dbRefID = gid()
    let h2data = {
      ast: FluidAST.ofExpr(
        EFnCall(gid(), "DB::deleteAll_v1", list{EVariable(dbRefID, "Books")}, NoRail),
      ),
      spec: {
        space: B.newF("HTTP"),
        name: B.newF("/hello"),
        modifier: B.newF("GET"),
      },
      hTLID: h2tlid,
      pos: {x: 0, y: 0},
    }

    let dbtlid = gtlid()
    let dbdata: PT.DB.t = {
      dbTLID: dbtlid,
      dbName: B.newF("Books"),
      cols: list{},
      version: 0,
      pos: {x: 0, y: 0},
    }

    let dbs = TD.fromList(list{(dbdata.dbTLID, dbdata)})
    let handlers = TD.fromList(list{(h1data.hTLID, h1data), (h2data.hTLID, h2data)})

    test("dbsByName", () =>
      expect(dbsByName(dbs)) |> toEqual(Map.add(~key="Books", ~value=dbtlid, Map.String.empty))
    )
    test("handlersByName", () => {
      let v = handlers |> handlersByName |> Map.get(~key="WORKER:processOrder")

      expect(v) |> toEqual(Some(h1tlid))
    })
    test("findUsagesInAST", () => {
      let handlers = handlersByName(handlers)
      let datastores = dbsByName(dbs)
      let functions = Map.String.empty
      let packageFunctions = Map.String.empty
      let usages = switch findUsagesInAST(
        h2tlid,
        ~datastores,
        ~handlers,
        ~functions,
        ~packageFunctions,
        h2data.ast,
      ) {
      | list{{refersTo, usedIn, id}} => refersTo == h2tlid && (usedIn == dbtlid && id === dbRefID)
      | _ => false
      }

      expect(usages) |> toEqual(true)
    })
    test("tlidsToUpdateUsage", () => {
      let fntlid = gtlid()
      let ops = list{
        SetHandler(h1tlid, {x: 0, y: 0}, h1data),
        SetExpr(h1tlid, gid(), EBlank(gid())),
        SetFunction({
          ufTLID: fntlid,
          ufMetadata: {
            ufmName: B.newF("trollClean"),
            ufmParameters: list{},
            ufmDescription: "can users put docs here?",
            ufmReturnTipe: B.new_(),
            ufmInfix: false,
          },
          ufAST: FluidAST.ofExpr(FluidExpression.newB()),
        }),
      }

      expect(tlidsToUpdateUsage(ops)) |> toEqual(list{h1tlid, fntlid})
    })
    test("updateAssocList from empty", () =>
      expect(
        updateAssocList(~key="a", list{}, ~f=u =>
          switch u {
          | Some(v) => Some(v)
          | None => Some(1)
          }
        ),
      ) |> toEqual(list{("a", 1)})
    )
    test("updateAssocList add non existing", () =>
      expect(
        updateAssocList(~key="b", list{("a", 1)}, ~f=u =>
          switch u {
          | Some(v) => Some(v)
          | None => Some(1)
          }
        ),
      ) |> toEqual(list{("a", 1), ("b", 1)})
    )
    ()
  })
  ()
}
