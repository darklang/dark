open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : fn list =
  [ { name = fn "Char" "toASCIICode" 0
    ; infix_names = []
    ; parameters = [par "c" TCharacter]
    ; return_type = TInt
    ; description = "Return `c`'s ASCII code"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true }
  ; { name = fn "Char" "toASCIIChar" 0
    ; infix_names = []
    ; parameters = [par "i" TInt]
    ; return_type = TCharacter
    ; description = "convert an int to an ASCII character"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true }
  ; { name = fn "Char" "toLowercase" 0
    ; infix_names = []
    ; parameters = [par "c" TCharacter]
    ; return_type = TCharacter
    ; description = "Return the lowercase value of `c`"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true }
  ; { name = fn "Char" "toUppercase" 0
    ; infix_names = []
    ; parameters = [par "c" TCharacter]
    ; return_type = TCharacter
    ; description = "Return the uppercase value of `c`"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true } ]
