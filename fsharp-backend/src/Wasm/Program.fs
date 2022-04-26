open System

open System.Threading.Tasks
open FSharp.Control.Tasks

open Wasm

let twoPlusThree =
  """
  [
      "AnalyzeHandler",
      {
          "handler": {
              "tlid": "163992584",
              "spec": {
                  "name": [
                      "Filled",
                      "173388941",
                      "violentTamarin"
                  ],
                  "module": [
                      "Filled",
                      "2107282711",
                      "REPL"
                  ],
                  "modifier": [
                      "Filled",
                      "2133308295",
                      "_"
                  ],
                  "types": {
                      "input": [
                          "Blank",
                          "848577556"
                      ],
                      "output": [
                          "Blank",
                          "1904777602"
                      ]
                  }
              },
              "ast": [
                  "EBinOp",
                  "133960504",
                  "+",
                  [
                      "EInteger",
                      "36273220",
                      "2"
                  ],
                  [
                      "EInteger",
                      "196615730",
                      "3"
                  ],
                  [
                      "NoRail"
                  ]
              ]
          },
          "trace_id": "7d495105-946f-5ad8-8db9-4fd70e6eff67",
          "trace_data": {
              "input": [],
              "timestamp": "1970-01-01T00:00:00Z",
              "function_results": []
          },
          "dbs": [],
          "user_fns": [],
          "user_tipes": [],
          "secrets": []
      }
  ]
  """

let listLengthRoundtrip =
  """
  [
      "AnalyzeHandler",
      {
          "handler": {
            "tlid": "1354237328",
            "spec": {
                "name": [
                    "Filled",
                    "1486155560",
                    "sinisterMonkey"
                ],
                "module": [
                    "Filled",
                    "830372985",
                    "REPL"
                ],
                "modifier": [
                    "Filled",
                    "1642623623",
                    "_"
                ],
                "types": {
                    "input": [
                        "Blank",
                        "1454144599"
                    ],
                    "output": [
                        "Blank",
                        "357833775"
                    ]
                }
            },
            "ast": [
                "EPipe",
                "2048653664",
                [
                    [
                        "EFnCall",
                        "180682653",
                        "List::repeat",
                        [
                            [
                                "EInteger",
                                "1214060656",
                                "10000"
                            ],
                            [
                                "EInteger",
                                "893215982",
                                "1"
                            ]
                        ],
                        [
                            "NoRail"
                        ]
                    ],
                    [
                        "EFnCall",
                        "416359115",
                        "List::length",
                        [
                            [
                                "EPipeTarget",
                                "923435829"
                            ]
                        ],
                        [
                            "NoRail"
                        ]
                    ]
                ]
            ]
          },
          "trace_id": "7d495105-946f-5ad8-8db9-4fd70e6eff67",
          "trace_data": {
              "input": [],
              "timestamp": "1970-01-01T00:00:00Z",
              "function_results": []
          },
          "dbs": [],
          "user_fns": [],
          "user_tipes": [],
          "secrets": []
      }
  ]
  """


[<EntryPoint>]
let main argv =
  printfn "Dark.wasm console app running"

  listLengthRoundtrip
  |> EvalWorker.HandleMessage
  |> Async.AwaitTask
  |> Async.RunSynchronously
  |> printfn "list roundtrip: %A"

  // twoPlusThree
  // |> EvalWorker.HandleMessage
  // |> Async.AwaitTask
  // |> Async.RunSynchronously
  // |> printfn "2+3: %A"



  0 // return an integer exit code
