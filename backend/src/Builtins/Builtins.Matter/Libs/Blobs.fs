/// The two blob builtins that touch the package STORE: promote an ephemeral blob
/// into `package_blobs`, and get a persistent ref back from a hash.
///
/// The pure blob builtins (`Builtins.Pure.Libs.Bytes`) never write the store and
/// never need to find a row by hash: an ephemeral blob carries its bytes, and a
/// persistent one is only ever handed to code by something that already read the
/// row. These two are what lets Dark code MAKE a persistent blob and NAME one, which
/// a generator needs: its input file is stored once, and the saved generator refers
/// to it by hash.
module Builtins.Matter.Libs.Blobs

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes


let fns () : List<BuiltInFn> =
  [ { name = fn "bytesPersist" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Stores <param bytes>'s bytes in the package store, content-addressed, and "
        + "returns the hash they live under, which is what `bytesLoad` takes. "
        + "Idempotent: the same bytes persist to the same hash. A blob that is "
        + "already persistent just reports its hash."
      fn =
        (function
        | state, _, _, [| DBytes ref |] ->
          uply {
            match ref with
            | Persistent(hash, _) -> return DString hash
            | Ephemeral _ ->
              // The leaf handler, not the whole-graph rewriter: this IS the leaf, and
              // going through `promote` meant matching a shape it can only return.
              match! Bytes.promoteEphemeralLeaf state.blobs.persist (DBytes ref) with
              | Some(DBytes(Persistent(hash, _))) -> return DString hash
              | _ ->
                return Exception.raiseInternal "an ephemeral blob did not promote" []
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "bytesLoad" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "a persistent blob's content hash" ]
      returnType = TypeReference.option TBytes
      description =
        "The persistent blob stored under <param hash>, or None when the store has "
        + "no such row."
      fn =
        (function
        | state, _, _, [| DString hash |] ->
          uply {
            // The ref needs the length, and the only place that knows it is the row,
            // so this reads the bytes once. A blob a generator feeds on is small;
            // this is not a streaming path.
            let! bytes = state.blobs.get hash
            return
              bytes
              |> Option.map (fun (bs : byte[]) ->
                DBytes(Persistent(hash, int64 bs.Length)))
              |> Dval.option KTBytes
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      // CLEANUP: whether this should carry `PackageRead` is unsettled; it reads a row
      // the program named by hash. Revisit alongside `pmTypeDeclaration`.
      //
      // Dereferencing a blob is value materialisation, not an effect: the pure blob
      // builtins read persistent bytes the same way with no call effect (see
      // docs/effects.md on the local-storage boundary). The difference worth stating,
      // because it is the one this argument has to cover: those builtins only ever see
      // a ref they were handed, while this one starts from a hash the program supplies,
      // so it can in principle name a blob the caller was never given. Guessing a
      // SHA-256 is infeasible, which is why the answer is "effect-free" rather than
      // "unreachable", and why it is a CLEANUP rather than settled.
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
