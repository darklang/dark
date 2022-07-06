module Tests.HttpQueryEncoding

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

open BackendOnlyStdLib

module RT = LibExecution.RuntimeTypes

open TestUtils.TestUtils


let toQueryTests =
  testList
    "toQuery"
    [ test "empty DObj works" {
        let queryObj = RT.DObj(Map.empty)

        let actual = HttpQueryEncoding.toQuery queryObj
        let expected = Ok []

        Expect.equal actual expected "old format"
      }

      test "DObj with single string works correctly" {
        let queryObj = RT.DObj(Map.ofList [ "key", RT.DStr "value" ])

        let actual = HttpQueryEncoding.toQuery queryObj
        let expected = Ok [ "key", [ "value" ] ]

        Expect.equal actual expected "old format"
      }

      test "DObj with tuple value formatted correctly" {
        let queryObj =
          RT.DObj(Map.ofList [ "key", RT.DTuple(RT.DInt 1, RT.DInt 2, []) ])

        let actual = HttpQueryEncoding.toQuery queryObj
        let expected = Ok [ "key", [ "( 1, 2 )" ] ] // TUPLETODO is this ideal?

        Expect.equal actual expected "old format"
      } ]

let tests = testList "HttpQueryEncoding" [ toQueryTests ]
