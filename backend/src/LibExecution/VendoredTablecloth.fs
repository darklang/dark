module VendoredTablecloth

module Result =
  let unwrapWith (f : 'err -> 'ok) (t : Result<'ok, 'err>) : 'ok =
    match t with
    | Ok v -> v
    | Error v -> f v

  [<CompilerMessageAttribute("Result.unwrapUnsafe is banned, use Prelude.Exception.unwrapResult* instead",
                             0,
                             IsError = true,
                             IsHidden = true)>]
  let unwrapUnsafe = Tablecloth.Result.unwrapUnsafe

module Option =

  [<CompilerMessageAttribute("Option.unwrapUnsafe is banned, use Prelude.Exception.unwrapOption* instead",
                             0,
                             IsError = true,
                             IsHidden = true)>]
  let unwrapUnsafe = Tablecloth.Option.unwrapUnsafe

  [<CompilerMessageAttribute("Option.get is banned, use Prelude.Exception.unwrapOption* instead",
                             0,
                             IsError = true,
                             IsHidden = true)>]
  let get = Option.get

  let unwrap (def : 'a) (o : Option<'a>) : 'a =
    match o with
    | None -> def
    | Some value -> value
