open Core_kernel

open Lib
open Runtime
open Types.RuntimeT


let fns : Lib.shortfn list = [
  (* ====================================== *)
  (* Password *)
  (* ====================================== *)
  { pns = ["Password::hash"]
  ; ins = []
  ; p = [par "pw" TStr]
  ; r = TPassword
  ; d = "Hash a password into a Password by salting and hashing it. \
         This uses libsodium's crypto_pwhash_str under the hood, which is \
         based on argon2."
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
  ;

  { pns = ["Password::check"]
  ; ins = []
  ; p = [par "existingpwr" TPassword; par "rawpw" TStr]
  ; r = TBool
  ; d = "Check whether a Password matches a raw password String \
         safely. This uses libsodium's pwhash under the hood, which is \
         based on argon2."
  ; f = NotClientAvailable
  ; pr = None
  ; ps = true
  ; dep = false
  }
]

