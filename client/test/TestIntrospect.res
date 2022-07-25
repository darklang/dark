open Prelude
open Tester
open Introspect
module TL = Toplevel
module B = BlankOr

let run = () => {
  describe("Introspect", () => {
    let h1tlid = gtlid()
    let h1data: PT.Handler.t = {
      ast: FluidAST.ofExpr(EBlank(gid())),
      spec: PT.Handler.Spec.newWorker("processOrder"),
      tlid: h1tlid,
      pos: {x: 0, y: 0},
    }

    let h2tlid = gtlid()
    let dbRefID = gid()
    let h2data: PT.Handler.t = {
      ast: FluidAST.ofExpr(
        EFnCall(
          gid(),
          Stdlib({module_: "DB", function: "deleteAll", version: 1}),
          list{EVariable(dbRefID, "Books")},
          NoRail,
        ),
      ),
      spec: PT.Handler.Spec.newHTTP("/hello", "GET"),
      tlid: h2tlid,
      pos: {x: 0, y: 0},
    }

    let dbtlid = gtlid()
    let dbdata: PT.DB.t = {
      tlid: dbtlid,
      name: B.newF("Books"),
      cols: list{},
      version: 0,
      pos: {x: 0, y: 0},
    }

    let dbs = TD.fromList(list{(dbdata.tlid, dbdata)})
    let handlers = TD.fromList(list{(h1data.tlid, h1data), (h2data.tlid, h2data)})

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
        PT.Op.SetHandler(h1tlid, {x: 0, y: 0}, h1data),
        SetExpr(h1tlid, gid(), EBlank(gid())),
        SetFunction({
          tlid: fntlid,
          metadata: {
            name: B.newF("trollClean"),
            parameters: list{},
            description: "can users put docs here?",
            returnType: B.new_(),
            infix: false,
          },
          ast: FluidAST.ofExpr(FluidExpression.newB()),
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
