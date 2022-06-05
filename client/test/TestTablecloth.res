open Tester
open! Tc

let run = () => {
  describe("List", () => {
    test("moveInto moves an element between two elements", () =>
      expect(list{0, 1, 2, 3, 4} |> List.moveInto(~oldPos=4, ~newPos=1)) |> toEqual(list{
        0,
        4,
        1,
        2,
        3,
      })
    )
    test("moveInto moves an element further down the list", () =>
      expect(
        list{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} |> List.moveInto(~oldPos=1, ~newPos=6),
      ) |> toEqual(list{0, 2, 3, 4, 5, 1, 6, 7, 8, 9})
    )
    test("moveInto moves an element further up the list", () =>
      expect(
        list{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} |> List.moveInto(~oldPos=6, ~newPos=1),
      ) |> toEqual(list{0, 6, 1, 2, 3, 4, 5, 7, 8, 9})
    )
    test("moveInto moves element to beginning of the list", () =>
      expect(list{0, 1, 2, 3, 4} |> List.moveInto(~oldPos=3, ~newPos=0)) |> toEqual(list{
        3,
        0,
        1,
        2,
        4,
      })
    )
    test("moveInto moves element to end of the list", () =>
      expect(list{0, 1, 2, 3, 4} |> List.moveInto(~oldPos=3, ~newPos=5)) |> toEqual(list{
        0,
        1,
        2,
        4,
        3,
      })
    )
    test("moveInto caps overflow", () =>
      expect(list{0, 1, 2, 3, 4} |> List.moveInto(~oldPos=2, ~newPos=7)) |> toEqual(list{
        0,
        1,
        3,
        4,
        2,
      })
    )
  })
  ()
}
