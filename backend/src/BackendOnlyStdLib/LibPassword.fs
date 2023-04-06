/// StdLib functions to hash and compare Passwords of Dark users
module BackendOnlyStdLib.LibPassword

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "Password" "hash" 0
      typeParams = []
      parameters = [ Param.make "password" TString "" ]
      returnType = TPassword
      description =
        "Hash a password into a Password by salting and hashing it. This uses libsodium's crypto_pwhash_str under the hood, which is based on argon2.
         NOTE: This is not usable interactively, because we do not send Password values to the client for security reasons."
      fn =
        (function
        | _, _, [ DStr s ] ->
          s
          // libsodium authors recommend the `interactive'
          // parameter set for interactive, online uses:
          // https://download.libsodium.org/doc/password_hashing/the_argon2i_function.html
          // and the general advice is to use the highest
          // numbers whose performance works for your use-case.
          // `interactive' takes about half a second on my laptop,
          // whereas the the `moderate' parameter set takes 3s
          // and the `sensitive' parameter set takes 12s.
          // -lizzie.

          // libsodium's crypto_pwhash_str, which is what this
          // calls eventually, transparently salts:
          // https://github.com/jedisct1/libsodium/blob/d49d7e8d4f4dd8df593beb9e715e7bc87bc74108/src/libsodium/crypto_pwhash/argon2/pwhash_argon2i.c#L187 *)
          |> Sodium.PasswordHash.ArgonHashString
          |> UTF8.toBytes
          |> Password
          |> DPassword
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "Password" "check" 0
      typeParams = []
      parameters =
        [ Param.make "password" TPassword ""; Param.make "rawPassword" TString "" ]
      returnType = TBool
      description =
        "Check whether a Password matches a raw password String safely. This uses libsodium's pwhash under the hood, which is based on argon2.
        NOTE: This is not usable interactively, because we do not send Password values to the client for security reasons."
      fn =
        (function
        | _, _, [ DPassword (Password existingpw); DStr rawpw ] ->
          Sodium.PasswordHash.ArgonHashStringVerify(existingpw, UTF8.toBytes rawpw)
          |> DBool
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
