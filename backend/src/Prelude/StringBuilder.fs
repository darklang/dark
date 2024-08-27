module StringBuilder

open System.Text

let append (sb : StringBuilder) (s: string): unit =
  sb.Append s |> ignore<StringBuilder>
