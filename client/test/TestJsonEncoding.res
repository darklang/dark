open Prelude

open Tester

let testRoundtrip = (decoder: Js.Json.t => 'a, encoder: 'a => Js.Json.t, name: string, v: 'a) =>
  test("roundtrip " ++ name, () =>
    expect((encoder(v), v)) |> toEqual((encoder(v), v |> encoder |> decoder))
  )

let run = () => {
  describe("decoding", () => {
    module Decode = Json_decode_extended
    test("infinity", () =>
      expect(Decode.decodeString(RT.Dval.decode, `[ "DFloat", "Infinity" ]`)) |> toEqual(
        Belt.Result.Ok(RT.Dval.DFloat(Float.infinity)),
      )
    )
    test("negativeInfinity", () =>
      expect(Decode.decodeString(RT.Dval.decode, `[ "DFloat", "-Infinity" ]`)) |> toEqual(
        Belt.Result.Ok(RT.Dval.DFloat(Float.negativeInfinity)),
      )
    )
    test("notANumber", () => {
      switch Decode.decodeString(RT.Dval.decode, `[ "DFloat", "NaN" ]`) {
      | Ok(DFloat(flt)) => expect(Float.isNaN(flt)) |> toEqual(true)
      | _ => expect("A valid dfloat dval") |> toEqual("something invalid, or not a dfloat")
      }
    })
    test("big tlid", () => {
      let encoded = `["DIncomplete",["SourceID","14219007199254740993","14219007199254740993"]]`
      expect(
        encoded
        |> Json.parse
        |> Option.unwrapUnsafe
        |> RT.Dval.decode
        |> RT.Dval.encode
        |> Js.Json.stringify,
      ) |> toEqual(encoded)
    })
  })
  describe("dval misc tests", () => {
    describe("compatible with server JSON encoding", () => {
      test("obj", () =>
        expect(`["DObj",{"foo":["DInt",5]}]`) |> toEqual(
          RT.Dval.obj(list{("foo", DInt(5L))}) |> RT.Dval.encode |> Js.Json.stringify,
        )
      )
      test("DHttpResponse shape", () =>
        expect(`["DHttpResponse",["Response",401,[],["DNull"]]]`) |> toEqual(
          RT.Dval.DHttpResponse(Response(401L, list{}, DNull))
          |> RT.Dval.encode
          |> Js.Json.stringify,
        )
      )
      test("nan shape", () =>
        expect(`["DFloat","NaN"]`) |> toEqual(
          RT.Dval.DFloat(Tc.Float.nan) |> RT.Dval.encode |> Js.Json.stringify,
        )
      )
    })
  })
  describe("dval roundtrips", () => {
    open RT.Dval
    let t = testRoundtrip(RT.Dval.decode, RT.Dval.encode)
    let id = UInt64.fromString("15223423459603010931")->Tc.Option.unwrapUnsafe
    let id2 = UInt64.fromString("14219007199254740993")->Tc.Option.unwrapUnsafe
    t("int", DInt(5L))
    t("int_max_31_bits", DInt(1073741823L)) // 2^30-1
    t("int_min_31_bits", DInt(-1073741824L)) // -2^30
    t("int_max_32_bits", DInt(2147483647L)) // 2^31-1
    t("int_min_32_bits", DInt(-2147483648L)) // 2^31-1
    t("int_max_63_bits", DInt(4611686018427387903L)) // 2^62-1
    t("int_min_63_bits", DInt(-4611686018427387904L)) // -2^62
    t("int_max_64_bits", DInt(9223372036854775807L)) // 2^63-1
    t("int_min_64_bits", DInt(-9223372036854775808L)) // -2^63
    t("int_max_double", DInt(9007199254740992L)) // 2^53
    t("int_min_double", DInt(-9007199254740992L)) // -2^53
    t("obj", DObj(Belt.Map.String.fromArray([("foo", DInt(5L))])))
    t("date", DDate("can be anything atm"))
    t("incomplete none", DIncomplete(SourceNone))
    t("incomplete id", DIncomplete(SourceID(TLID.fromUInt64(id), ID.fromUInt64(id))))
    t("incomplete id2", DIncomplete(SourceID(TLID.fromUInt64(id2), ID.fromUInt64(id2))))
    t("float", DFloat(7.2))
    t("infinity", DFloat(Float.infinity))
    t("-infinity", DFloat(Float.negativeInfinity))
    // no test for nan as they aren't equal
    t("true", DBool(true))
    t("false", DBool(false))
    t("tuple", DTuple(DInt(56L), DIncomplete(SourceNone), list{DInt(78L)}))
    t("string", DStr("incredibly this was broken"))
    t("string", DStr("incredibly 😀"))
    t("null", DNull)
    t("option none", DOption(None))
    t("option just", DOption(Some(DInt(5L))))
    t("result ok", DResult(Ok(DInt(5L))))
    t("result err", DResult(Error(DInt(5L))))
    t("errorrail", DErrorRail(DInt(5L)))
    t("db", DDB("Visitors"))
    t("list", DList(list{DDB("Visitors"), DInt(4L)}))
    t("redirect", DHttpResponse(Redirect("/home")))
    t("httpresponse", DHttpResponse(Response(200L, list{}, DStr("success"))))
    t("httpredirect", DHttpResponse(Redirect("success")))
  })

  describe("tlid roundtrips", () => {
    let t = testRoundtrip(TLID.decode, TLID.encode)
    t("zero", TLID.fromInt(0))
    t("one", TLID.fromInt(1))
    t("two", TLID.fromInt(2))
    t("int_max_31_bits", TLID.fromInt(1073741823)) // 2^30-1
    t("int_max_32_bits", TLID.fromInt(2147483647)) // 2^31-1
    t("int_max_63_bits", TLID.fromString("4611686018427387903")->Option.unwrapUnsafe) // 2^62-1
    t("int_max_64_bits", TLID.fromString("9223372036854775807")->Option.unwrapUnsafe) // 2^63-1
    t("int_max_double", TLID.fromString("9007199254740992")->Option.unwrapUnsafe) // 2^53
    t("above uint63max", TLID.fromString("15223423459603010931")->Option.unwrapUnsafe)
    t("bug 1", TLID.fromString("14219007199254740993")->Option.unwrapUnsafe)
    t("bug 2", TLID.fromString("18446744073709551615")->Option.unwrapUnsafe)
  })

  describe("fluidExpr", () => {
    let t = testRoundtrip(PT.Expr.decode, PT.Expr.encode)
    t("complex", FluidTestData.complexExpr)
  })

  describe("tipe", () => {
    let roundtrip = typ => typ |> DType.encode |> DType.decode

    test("tuple tipe roundtrips", () => {
      let tipe = DType.TTuple(TInt, TFloat, list{TIncomplete})
      expect(tipe |> roundtrip) |> toEqual(tipe)
    })
  })
  describe("decode serializations", () => {
    let t = (filename, decoder, encoder) => {
      test(`decoding ${filename}`, () => {
        let contents =
          NodeJs.Fs.readFileSync(`backend/serialization/${filename}`) |> NodeJs.Buffer.toString
        expect(contents |> Js.Json.parseExn |> decoder |> encoder) |> toEqual(
          contents |> Js.Json.parseExn,
        )
      })
    }
    t(
      "vanilla-ApiServer-DBs-DBStatsV1-Params-simple.json",
      APIDBs.DBStats.Params.decode,
      APIDBs.DBStats.Params.encode,
    )
    t(
      "vanilla-ApiServer-DBs-Unlocked-T-simple.json",
      APIDBs.UnlockedDBs.decode,
      APIDBs.UnlockedDBs.encode,
    )
    t(
      "vanilla-ApiServer-Execution-FunctionV1-Params-simple.json",
      APIExecution.Function.Params.decode,
      APIExecution.Function.Params.encode,
    )
    t(
      "vanilla-ApiServer-Execution-FunctionV1-T-simple.json",
      APIExecution.Function.decode,
      APIExecution.Function.encode,
    )
    t(
      "vanilla-ApiServer-Execution-HandlerV1-Params-simple.json",
      APIExecution.Handler.Params.decode,
      APIExecution.Handler.Params.encode,
    )
    t(
      "vanilla-ApiServer-Execution-HandlerV1-T-simple.json",
      APIExecution.Handler.decode,
      APIExecution.Handler.encode,
    )
    t(
      "vanilla-ApiServer-F404s-Delete-Params-simple.json",
      API404.Delete.Params.decode,
      API404.Delete.Params.encode,
    )
    // No test because we don't check the response
    // t("vanilla-ApiServer-F404s-Delete-T-simple.json", API404.Delete.decode, API404.Delete.encode)
    t("vanilla-ApiServer-F404s-List-T-simple.json", API404.List.decode, API404.List.encode)
    t(
      "vanilla-ApiServer-InitialLoad-V1-T-initial.json",
      APIInitialLoad.decode,
      APIInitialLoad.encode,
    )
    t(
      "vanilla-ApiServer-Secrets-InsertV1-Secret-simple.json",
      APISecrets.Insert.Params.decode,
      APISecrets.Insert.Params.encode,
    )
    t(
      "vanilla-ApiServer-Secrets-InsertV1-T-simple.json",
      APISecrets.Insert.decode,
      APISecrets.Insert.encode,
    )
    t(
      "vanilla-ApiServer-Toplevels-Delete-Params-simple.json",
      APIToplevels.DeleteForever.Params.decode,
      APIToplevels.DeleteForever.Params.encode,
    )
    t(
      "vanilla-ApiServer-Traces-AllTraces-T-simple.json",
      APITraces.AllTraces.decode,
      APITraces.AllTraces.encode,
    )
    t(
      "vanilla-ApiServer-Traces-TraceDataV1-Params-simple.json",
      APITraces.TraceData.Params.decode,
      APITraces.TraceData.Params.encode,
    )
    t(
      "vanilla-ApiServer-Traces-TraceDataV1-T-simple.json",
      APITraces.TraceData.decode,
      APITraces.TraceData.encode,
    )
    t(
      "vanilla-ApiServer-Workers-Scheduler-Params-simple.json",
      APIWorkers.Scheduler.Params.decode,
      APIWorkers.Scheduler.Params.encode,
    )
    t(
      "vanilla-ApiServer-Workers-WorkerStats-Params-simple.json",
      APIWorkers.WorkerStats.Params.decode,
      APIWorkers.WorkerStats.Params.encode,
    )
    t(
      "vanilla-ApiServer-Workers-WorkerStats-T-simple.json",
      APIWorkers.WorkerStats.decode,
      APIWorkers.WorkerStats.encode,
    )
    t(
      "vanilla-ClientTypes-Analysis-PerformAnalysisParams-handler.json",
      AnalysisTypes.PerformAnalysis.Params.decode,
      AnalysisTypes.PerformAnalysis.Params.encode,
    )
    t(
      "vanilla-ClientTypes-Analysis-PerformAnalysisParams-function.json",
      AnalysisTypes.PerformAnalysis.Params.decode,
      AnalysisTypes.PerformAnalysis.Params.encode,
    )
    t("vanilla-LibBackend-Op-AddOpResultV1-simple.json", APIAddOps.decode, APIAddOps.encode)
    t(
      "vanilla-LibBackend-Op-AddOpParamsV1-simple.json",
      APIAddOps.Params.decode,
      APIAddOps.Params.encode,
    )
    t(
      "vanilla-LibBackend-StaticAssets-StaticDeploy-simple.json",
      StaticAssets.Deploy.decode,
      StaticAssets.Deploy.encode,
    )
    t("vanilla-LibExecution-ProgramTypes-Position-simple.json", Pos.decode, Pos.encode)
    t("vanilla-ClientTypes-Dval-T-complete.json", RT.Dval.decode, RT.Dval.encode)
    t(
      "vanilla-Microsoft-FSharp-Collections-FSharpList-1-ApiServer-Functions-BuiltInFn-T-all.json",
      Json.Decode.list(RT.BuiltInFn.decode),
      Json.Encode.list(RT.BuiltInFn.encode),
    )
    t(
      "vanilla-Microsoft-FSharp-Collections-FSharpList-1-LibExecution-ProgramTypes-Package-Fn-simple.json",
      Json.Decode.list(ProgramTypes.Package.Fn.decode),
      Json.Encode.list(ProgramTypes.Package.Fn.encode),
    )
    // t(
    //   "vanilla-Microsoft-FSharp-Collections-FSharpMap-2-System-String-ApiServer-DBs-DBStatsV1-Stat-simple.json",
    //   TODO.decode,
    //   TODO.encode,
    // )
    // t(
    //   "vanilla-Microsoft-FSharp-Collections-FSharpMap-2-System-String-System-String-simple.json",
    //   TODO.decode,
    //   TODO.encode,
    // )
    // t(
    //   "vanilla-Microsoft-FSharp-Core-FSharpResult-2-System-Tuple-2-System-Guid-System-Collections-Generic-Dictionary-2-System-UInt64-LibExecution-AnalysisTypes-ExecutionResult-System-String-simple.json",
    //   TODO.decode,
    //   TODO.encode,
    // )
    // t("vanilla-Prelude-pos-simple.json", TODO.decode, TODO.encode)
    // t(
    //   "vanilla-System-Tuple-2-System-Guid-Microsoft-FSharp-Collections-FSharpList-1-System-UInt64-simple.json",
    //   TODO.decode,
    //   TODO.encode,
    // )
    // t(
    //   "vanilla-System-Tuple-5-System-String-System-String-System-String-NodaTime-Instant-System-Guid-simple.json",
    //   TODO.decode,
    //   TODO.encode,
    // )
  })
}
