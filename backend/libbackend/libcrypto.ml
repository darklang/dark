open Core_kernel
open Libexecution
open Lib
open Runtime
open Types.RuntimeT
module Hash = Sodium.Password_hash.Bytes

let digest_to_bytes (digest : Nocrypto.Hash.digest) :
    Libexecution.Types.RuntimeT.RawBytes.t =
  let len = Cstruct.len digest in
  let bytes = Bytes.create len in
  Cstruct.blit_to_bytes digest 0 bytes 0 len ;
  bytes


let fns : Lib.shortfn list =
  [ (* ====================================== *)
    (* Password *)
    (* ====================================== *)
    { pns = ["Password::hash"]
    ; ins = []
    ; p = [par "pw" TStr]
    ; r = TPassword
    ; d =
        "Hash a password into a Password by salting and hashing it. This uses libsodium's crypto_pwhash_str under the hood, which is based on argon2.
        NOTE: This is not usable interactively, because we do not send Password values to the client for security reasons."
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              s
              |> Unicode_string.to_string
              |> Bytes.of_string
              (* wipe_to_password is a confusing name
                                but it's the only way to get a `password'
                                from a `bytes'. It also wipes the `bytes',
                                but passwords in memory is a little outside
                                of our threat model right now. *)
              |> Hash.wipe_to_password
              (* libsodium authors recommend the `interactive'
                                parameter set for interactive, online uses:
                                https://download.libsodium.org/doc/password_hashing/the_argon2i_function.html
                                and the general advice is to use the highest
                                numbers whose performance works for your use-case.
                                `interactive' takes about half a second on my laptop,
                                whereas the the `moderate' parameter set takes 3s
                                and the `sensitive' parameter set takes 12s.
                                -lizzie.
                              *)
              (* libsodium's crypto_pwhash_str, which is what this
                                calls eventually, transparently salts:
                                https://github.com/jedisct1/libsodium/blob/d49d7e8d4f4dd8df593beb9e715e7bc87bc74108/src/libsodium/crypto_pwhash/argon2/pwhash_argon2i.c#L187 *)
              |> Hash.hash_password Sodium.Password_hash.interactive
              |> DPassword
          | args ->
              fail args)
    ; ps = false
    ; dep = false
    }
  ; { pns = ["Password::check"]
    ; ins = []
    ; p = [par "existingpwr" TPassword; par "rawpw" TStr]
    ; r = TBool
    ; d =
        "Check whether a Password matches a raw password String safely. This uses libsodium's pwhash under the hood, which is based on argon2.
        NOTE: This is not usable interactively, because we do not send Password values to the client for security reasons."
    ; f =
        InProcess
          (function
          | _, [DPassword existingpw; DStr rawpw] ->
              rawpw
              |> Unicode_string.to_string
              |> Bytes.of_string
              |> Hash.wipe_to_password
              |> Hash.verify_password_hash existingpw
              |> DBool
          | args ->
              fail args)
    ; ps = false
    ; dep = false
    }
  ; { pns = ["Crypto::sha256"]
    ; ins = []
    ; p = [par "data" TBytes]
    ; r = TBytes
    ; d = "Computes the SHA-256 digest of the given `data`."
    ; f =
        InProcess
          (function
          | _, [DBytes data] ->
              Cstruct.of_bytes data
              |> Nocrypto.Hash.SHA256.digest
              |> digest_to_bytes
              |> DBytes
          | args ->
              fail args)
    ; ps = false
    ; dep = false
    }
  ; { pns = ["Crypto::sha384"]
    ; ins = []
    ; p = [par "data" TBytes]
    ; r = TBytes
    ; d = "Computes the SHA-384 digest of the given `data`."
    ; f =
        InProcess
          (function
          | _, [DBytes data] ->
              Cstruct.of_bytes data
              |> Nocrypto.Hash.SHA384.digest
              |> digest_to_bytes
              |> DBytes
          | args ->
              fail args)
    ; ps = false
    ; dep = false
    }
  ; { pns = ["Crypto::sha256hmac"]
    ; ins = []
    ; p = [par "key" TBytes; par "data" TBytes]
    ; r = TBytes
    ; d =
        "Computes the 256 HMAC (hash-based message authentication code) digest of the given `key` and `data`."
    ; f =
        InProcess
          (function
          | _, [DBytes key; DBytes data] ->
              let key = Cstruct.of_bytes key in
              let data = Cstruct.of_bytes data in
              Nocrypto.Hash.SHA256.hmac ~key data |> digest_to_bytes |> DBytes
          | args ->
              fail args)
    ; ps = false
    ; dep = false
    } ]
