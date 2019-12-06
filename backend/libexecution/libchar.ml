open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["Char::toASCIICode"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TInt
    ; d = "Return `c`'s ASCII code"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["Char::toASCIIChar"]
    ; ins = []
    ; p = [par "i" TInt]
    ; r = TCharacter
    ; d = "convert an int to an ASCII character"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["Char::toLowercase"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TCharacter
    ; d = "Return the lowercase value of `c`"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["Char::toUppercase"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TCharacter
    ; d = "Return the uppercase value of `c`"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true } ]
