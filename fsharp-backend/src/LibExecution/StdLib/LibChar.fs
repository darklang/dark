open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : fn list =
  [ { name = fn "Char" "toASCIICode" 0

    ; parameters = [Param.make "c" TCharacter]
    ; return_type = TInt
    ; description = "Return `c`'s ASCII code"
    ; fn =
         (fun _ -> Exception.code "This function no longer exists.")
    ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Char" "toASCIIChar" 0

    ; parameters = [Param.make "i" TInt]
    ; return_type = TCharacter
    ; description = "convert an int to an ASCII character"
    ; fn =
         (fun _ -> Exception.code "This function no longer exists.")
    ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Char" "toLowercase" 0

    ; parameters = [Param.make "c" TCharacter]
    ; return_type = TCharacter
    ; description = "Return the lowercase value of `c`"
    ; fn =
         (fun _ -> Exception.code "This function no longer exists.")
    ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Char" "toUppercase" 0

    ; parameters = [Param.make "c" TCharacter]
    ; return_type = TCharacter
    ; description = "Return the uppercase value of `c`"
    ; fn =
         (fun _ -> Exception.code "This function no longer exists.")
    ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) } ]
