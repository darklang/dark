/// The types that are serialized for the program
module LibExecution.ProgramTypes

open Prelude
open VendoredTablecloth

// Used for conversion functions
module RT = RuntimeTypes

module FQFnName =
  type T = RT.FQFnName.T
  type StdlibFnName = RT.FQFnName.StdlibFnName
  type PackageFnName = RT.FQFnName.PackageFnName
  type UserFnName = RT.FQFnName.UserFnName

  let packageFnName = RT.FQFnName.packageFnName
  let userFnName = RT.FQFnName.userFnName
  let stdlibFnName = RT.FQFnName.stdlibFnName
  let packageFqName = RT.FQFnName.packageFqName
  let userFqName = RT.FQFnName.userFqName
  let stdlibFqName = RT.FQFnName.stdlibFqName

  let oneWordFunctions =
    Set [ "toString"
          "toRepr"
          "equals"
          "notEquals"
          "assoc"
          "dissoc"
          "toForm"
          "emit"
          "toString_v0"
          "toRepr_v0"
          "equals_v0"
          "notEquals_v0"
          "assoc_v0"
          "dissoc_v0"
          "toForm_v0"
          "emit_v0"
          "emit_v1" ]

  let parse (fnName : string) : T =
    // These should match up with the regexes in RuntimeTypes
    match fnName with
    | Regex "^([a-z][a-z0-9_]*)/([a-z][a-z0-9A-Z]*)/([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
            [ owner; package; module_; name; version ] ->
      RT.FQFnName.packageFqName owner package module_ name (int version)
    | Regex "^([a-z][a-z0-9_]*)/([a-z][a-z0-9A-Z]*)/([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)$"
            [ owner; package; module_; name ] ->
      RT.FQFnName.packageFqName owner package module_ name 0
    | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
            [ module_; name; version ] ->
      RT.FQFnName.stdlibFqName module_ name (int version)
    | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)_v(\d+)$"
            [ module_; name; version ] ->
      RT.FQFnName.stdlibFqName module_ name (int version)
    | Regex "^([A-Z][a-z0-9A-Z_]*)::([a-z][a-z0-9A-Z_]*)$" [ module_; name ] ->
      RT.FQFnName.stdlibFqName module_ name 0
    | Regex "^([a-z][a-z0-9A-Z_]*)_v(\d+)$" [ name; version ] ->
      RT.FQFnName.stdlibFqName "" name (int version)
    | Regex "^Date::([-+><&|!=^%/*]{1,2})$" [ name ] ->
      RT.FQFnName.stdlibFqName "Date" name 0
    | Regex "^([-+><&|!=^%/*]{1,2})$" [ name ] -> RT.FQFnName.stdlibFqName "" name 0
    | Regex "^([-+><&|!=^%/*]{1,2})_v(\d+)$" [ name; version ] ->
      RT.FQFnName.stdlibFqName "" name (int version)
    // don't accidentally parse these as userFns
    | v when Set.contains v oneWordFunctions ->
      match v with
      | Regex "^([a-z][a-z0-9A-Z]*)_v(\d+)$" [ name; version ] ->
        RT.FQFnName.stdlibFqName "" name (int version)
      | Regex "^([a-z][a-z0-9A-Z]*)$" [ name ] -> RT.FQFnName.stdlibFqName "" name 0
      | _ ->
        Exception.raiseInternal
          "Bad format in one word function name"
          [ "fnName", fnName ]
    | Regex "^([a-z][a-z0-9A-Z_]*)$" [ name ] -> RT.FQFnName.userFqName name
    // CLEANUP People have the most ridiculous names in userFunctions. One user had a
    // fully qualified url in there! Ridiculous. This needs a data cleanup before it
    // can be removed.
    | Regex "^(.*)$" [ name ] -> RT.FQFnName.userFqName name
    | _ -> Exception.raiseInternal "Bad format in function name" [ "fnName", fnName ]




/// Expressions - the main part of the language.
type Expr =
  | EInteger of id * int64
  | EBool of id * bool
  | EString of id * string
  | ECharacter of id * string
  // Allow the user to have arbitrarily big numbers, even if they don't make sense as
  // floats. The float is split as we want to preserve what the user entered.
  // Strings are used as numbers lose the leading zeros (eg 7.00007)
  | EFloat of id * Sign * string * string
  | ENull of id
  | EBlank of id
  | ELet of id * string * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EBinOp of id * FQFnName.T * Expr * Expr * SendToRail
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * List<Expr> * SendToRail
  | EPartial of id * string * Expr
  | ERightPartial of id * string * Expr
  | ELeftPartial of id * string * Expr
  | EList of id * List<Expr>
  | ERecord of id * List<string * Expr>
  | EPipe of id * Expr * Expr * List<Expr>
  | EConstructor of id * string * List<Expr>
  | EMatch of id * Expr * List<Pattern * Expr>
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr

  member this.testEqualIgnoringIDs(other : Expr) : bool =
    (* helpers for recursive calls *)
    let eq (e : Expr) (e' : Expr) = e.testEqualIgnoringIDs (e')

    let eqList l1 l2 = List.length l1 = List.length l2 && List.forall2 eq l1 l2

    match this, other with
    // expressions with no values
    | ENull _, ENull _
    | EBlank _, EBlank _
    | EPipeTarget _, EPipeTarget _ -> true
    // expressions with single string values
    | EString (_, v), EString (_, v')
    | ECharacter (_, v), ECharacter (_, v')
    | EVariable (_, v), EVariable (_, v') -> v = v'
    | EInteger (_, v), EInteger (_, v') -> v = v'
    | EFloat (_, s, w, f), EFloat (_, s', w', f') -> s = s' && w = w' && f = f'
    | EBool (_, v), EBool (_, v') -> v = v'
    | ELet (_, lhs, rhs, body), ELet (_, lhs', rhs', body') ->
      lhs = lhs' && eq rhs rhs' && eq body body'
    | EIf (_, con, thn, els), EIf (_, con', thn', els') ->
      eq con con' && eq thn thn' && eq els els'
    | EList (_, l), EList (_, l') -> eqList l l'
    | EFnCall (_, name, args, toRail), EFnCall (_, name', args', toRail') ->
      name = name' && eqList args args' && toRail = toRail'
    | EBinOp (_, name, lhs, rhs, toRail), EBinOp (_, name', lhs', rhs', toRail') ->
      name = name' && eq lhs lhs' && eq rhs rhs' && toRail = toRail'
    | ERecord (_, pairs), ERecord (_, pairs') ->
      let sort = List.sortBy fst

      List.forall2
        (fun (k, v) (k', v') -> k = k' && eq v v')
        (sort pairs)
        (sort pairs')
    | EFieldAccess (_, e, f), EFieldAccess (_, e', f') -> eq e e' && f = f'
    | EPipe (_, e1, e2, l), EPipe (_, e1', e2', l') ->
      eqList l l' && eq e1 e1' && eq e2 e2'
    | EFeatureFlag (_, _, cond, old, knew), EFeatureFlag (_, _, cond', old', knew') ->
      eq cond cond' && eq old old' && eq knew knew'
    | EConstructor (_, s, ts), EConstructor (_, s', ts') -> s = s' && eqList ts ts'
    | ERightPartial (_, str, e), ERightPartial (_, str', e')
    | ELeftPartial (_, str, e), ELeftPartial (_, str', e')
    | EPartial (_, str, e), EPartial (_, str', e') -> str = str' && eq e e'
    | ELambda (_, vars, e), ELambda (_, vars', e') ->
      eq e e' && List.forall2 (fun (_, v) (_, v') -> v = v') vars vars'
    | EMatch (_, e, branches), EMatch (_, e', branches') ->
      eq e e'
      && List.forall2
           (fun ((p, v) : Pattern * Expr) (p', v') ->
             p.testEqualIgnoringIDs (p') && eq v v')
           branches
           branches'
    | ENull _, _
    | EBlank _, _
    | EPipeTarget _, _
    | EInteger _, _
    | EString _, _
    | ECharacter _, _
    | EVariable _, _
    | EBool _, _
    | EFloat _, _
    | ELet _, _
    | EIf _, _
    | EList _, _
    | EFnCall _, _
    | EBinOp _, _
    | ERecord _, _
    | EFieldAccess _, _
    | EPipe _, _
    | EFeatureFlag _, _
    | EConstructor _, _
    | ELeftPartial _, _
    | ERightPartial _, _
    | EPartial _, _
    | ELambda _, _
    | EMatch _, _ ->
      (* exhaustiveness check *)
      false

and SendToRail =
  | Rail
  | NoRail

and Pattern =
  | PVariable of id * string
  | PConstructor of id * string * List<Pattern>
  | PInteger of id * int64
  | PBool of id * bool
  | PCharacter of id * string
  | PString of id * string
  | PFloat of id * Sign * string * string
  | PNull of id
  | PBlank of id

  member this.testEqualIgnoringIDs(other : Pattern) : bool =
    let eq (a : Pattern) (b : Pattern) = a.testEqualIgnoringIDs (b)

    let eqList l1 l2 = List.length l1 = List.length l2 && List.forall2 eq l1 l2

    match (this, other) with
    | PVariable (_, name), PVariable (_, name') -> name = name'
    | (PConstructor (_, name, patterns), PConstructor (_, name', patterns')) ->
      name = name' && eqList patterns patterns'
    | PString (_, str), PString (_, str') -> str = str'
    | PInteger (_, l), PInteger (_, l') -> l = l'
    | PFloat (_, s, w, f), PFloat (_, s', w', f') -> (s, w, f) = (s', w', f')
    | PBool (_, l), PBool (_, l') -> l = l'
    | PCharacter (_, c), PCharacter (_, c') -> c = c'
    | PNull (_), PNull (_) -> true
    | PBlank (_), PBlank (_) -> true
    | PVariable _, _
    | PConstructor _, _
    | PString _, _
    | PInteger _, _
    | PFloat _, _
    | PBool _, _
    | PCharacter _, _
    | PNull _, _
    | PBlank _, _ -> false

type DType =
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TList of DType
  | TDict of DType
  | TIncomplete
  | TError
  | THttpResponse of DType
  | TDB of DType
  | TDate
  | TChar
  | TPassword
  | TUuid
  | TOption of DType
  | TErrorRail
  | TUserType of string * int
  | TBytes
  | TResult of DType * DType
  // A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string // replaces TAny
  | TFn of List<DType> * DType // replaces TLambda
  | TRecord of List<string * DType>
  | TDbList of DType // TODO: cleanup and remove
  // This allows you to build up a record to eventually be the right shape.
  // | TRecordWithFields of List<string * DType>
  // | TRecordPlusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType
  // | TRecordMinusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType

  static member parse(str : string) : Option<DType> =
    let any = TVariable "a"

    match String.toLowercase str with
    | "any" -> Some any
    | "int" -> Some TInt
    | "integer" -> Some TInt
    | "float" -> Some TFloat
    | "bool" -> Some TBool
    | "boolean" -> Some TBool
    | "nothing" -> Some TNull
    | "character"
    | "char" -> Some TChar
    | "str" -> Some TStr
    | "string" -> Some TStr
    | "list" -> Some(TList any)
    | "obj" -> Some(TDict any)
    | "block" -> Some(TFn([ TVariable "a" ], TVariable "b"))
    | "incomplete" -> Some TIncomplete
    | "error" -> Some TError
    | "response" -> Some(THttpResponse any)
    | "datastore" -> Some(TDB any)
    | "date" -> Some TDate
    | "password" -> Some TPassword
    | "uuid" -> Some TUuid
    | "option" -> Some(TOption any)
    | "errorrail" -> Some TErrorRail
    | "result" -> Some(TResult(TVariable "a", TVariable "b"))
    | "dict" -> Some(TDict any)
    | _ ->
      let parseListTyp (listTyp : string) : Option<DType> =
        match String.toLowercase listTyp with
        | "str" -> Some(TDbList TStr)
        | "string" -> Some(TDbList TStr)
        | "int" -> Some(TDbList TInt)
        | "integer" -> Some(TDbList TInt)
        | "float" -> Some(TDbList TFloat)
        | "bool" -> Some(TDbList TBool)
        | "boolean" -> Some(TDbList TBool)
        | "password" -> Some(TDbList TPassword)
        | "uuid" -> Some(TDbList TUuid)
        | "dict" -> Some(TDbList(TDict any))
        | "date" -> Some(TDbList TDate)
        | "title" -> Some(TDbList TStr)
        | "url" -> Some(TDbList TStr)
        | _ -> None

      if String.startsWith "[" str && String.endsWith "]" str then
        str |> String.dropLeft 1 |> String.dropRight 1 |> parseListTyp
      else
        None


module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

    override this.ToString() : string =
      match this with
      | EveryDay -> "Daily"
      | EveryWeek -> "Weekly"
      | EveryFortnight -> "Fortnightly"
      | EveryHour -> "Every 1hr"
      | Every12Hours -> "Every 12hrs"
      | EveryMinute -> "Every 1min"

    static member parse(modifier : string) : Option<CronInterval> =
      match String.toLowercase modifier with
      | "daily" -> Some EveryDay
      | "weekly" -> Some EveryWeek
      | "fortnightly" -> Some EveryFortnight
      | "every 1hr" -> Some EveryHour
      | "every 12hrs" -> Some Every12Hours
      | "every 1min" -> Some EveryMinute
      | _ -> None



  // We need to keep the IDs around until we get rid of them on the client
  type ids = { moduleID : id; nameID : id; modifierID : id }

  [<MessagePack.MessagePackObject>]
  type Spec =
    | HTTP of route : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    // Deprecated but still supported form
    // CLEANUP: convert these into regular workers (change module name to WORKER,
    // check if they're unique first though)
    | OldWorker of modulename : string * name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids
    // If there's no module
    // CLEANUP: convert these into repl and get rid of this case
    | UnknownHandler of name : string * modifier : string * ids : ids

    member this.name() =
      match this with
      | HTTP (route, _method, _ids) -> route
      | Worker (name, _ids) -> name
      | OldWorker (_modulename, name, _ids) -> name
      | Cron (name, interval, _ids) -> name
      | REPL (name, _ids) -> name
      | UnknownHandler (name, _modifier, _ids) -> name

    member this.modifier() =
      match this with
      | HTTP (_route, method, _ids) -> method
      | Worker (_name, _ids) -> "_"
      | OldWorker (_modulename, _name, _ids) -> "_"
      | Cron (_name, interval, _ids) ->
        interval |> Option.map string |> Option.defaultValue ""
      | REPL (_name, _ids) -> "_"
      | UnknownHandler (name, modifier, ids) -> modifier

    member this.module'() =
      match this with
      | HTTP _ -> "HTTP"
      | Worker _ -> "WORKER" // CLEANUP the DB relies on the casing
      | OldWorker (modulename, _name, _ids) -> modulename
      | Cron _ -> "CRON" // CLEANUP the DB relies on the casing
      | REPL _ -> "REPL"
      | UnknownHandler (name, modifier, ids) -> ""

    member this.complete() : bool =
      match this with
      | HTTP ("", _, _) -> false
      | HTTP (_, "", _) -> false
      | Worker ("", _) -> false
      | OldWorker ("", _, _) -> false
      | OldWorker (_, "", _) -> false
      | Cron ("", _, _) -> false
      | Cron (_, None, _) -> false
      | REPL ("", _) -> false
      | UnknownHandler _ -> false
      | _ -> true

    // Same as a TraceInput.EventDesc
    member this.toEventDesc() : Option<string * string * string> =
      if this.complete () then
        Some(this.module' (), this.name (), this.modifier ())
      else
        None

  type T = { tlid : tlid; pos : pos; ast : Expr; spec : Spec }


module DB =
  type Col = { name : Option<string>; typ : Option<DType>; nameID : id; typeID : id }

  type T =
    { tlid : tlid
      pos : pos
      nameID : id
      name : string
      version : int
      cols : List<Col> }

module UserType =
  type RecordField = { name : string; typ : Option<DType>; nameID : id; typeID : id }

  type Definition = Record of List<RecordField>

  type T =
    { tlid : tlid
      name : string
      nameID : id
      version : int
      definition : Definition }

module UserFunction =
  type Parameter =
    { name : string
      nameID : id
      typ : Option<DType>
      typeID : id
      description : string }

  type T =
    { tlid : tlid
      name : string
      nameID : id
      parameters : List<Parameter>
      returnType : DType
      returnTypeID : id
      description : string
      infix : bool
      body : Expr }

type Toplevel =
  | TLHandler of Handler.T
  | TLDB of DB.T
  | TLFunction of UserFunction.T
  | TLType of UserType.T

  member this.toTLID() : tlid =
    match this with
    | TLHandler h -> h.tlid
    | TLDB db -> db.tlid
    | TLFunction f -> f.tlid
    | TLType t -> t.tlid

  member this.toDBTypeString() =
    match this with
    | TLDB _ -> "db"
    | TLHandler _ -> "handler"
    | TLFunction _ -> "user_function"
    | TLType _ -> "user_tipe"

module Secret =
  type T = { name : string; value : string }


type DeprecatedMigrationKind = | DeprecatedMigrationKind

/// An Operation on a Canvas
///
/// "Op" is an abbreviation for Operation,
/// and is preferred throughout code and documentation.


open MessagePack
open MessagePack.Resolvers
open MessagePack.FSharp

[<MessagePackObject>]
type Op =
  | SetHandler of tlid * pos * Handler.T
  | CreateDB of tlid * pos * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid
  | MoveTL of tlid * pos
  | SetFunction of UserFunction.T
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | SetExpr of tlid * id * Expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid
  | CreateDBMigration of tlid * id * id * (string * id * string * id) list
  | AddDBColToDBMigration of tlid * id * id
  | SetDBColNameInDBMigration of tlid * id * string
  | SetDBColTypeInDBMigration of tlid * id * string
  | AbandonDBMigration of tlid
  | DeleteColInDBMigration of tlid * id
  | DeleteDBCol of tlid * id
  | DeprecatedInitDBm of tlid * id * id * id * DeprecatedMigrationKind
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * pos * id * string
  | DeleteTLForever of tlid
  | DeleteFunctionForever of tlid
  | SetType of UserType.T
  | DeleteType of tlid
  | DeleteTypeForever of tlid

type Oplist = List<Op>
type TLIDOplists = List<tlid * Oplist>

module Package =
  type Parameter = { name : string; typ : DType; description : string }

  type Fn =
    { name : FQFnName.PackageFnName
      body : Expr
      parameters : List<Parameter>
      returnType : DType
      description : string
      author : string
      deprecated : bool
      tlid : tlid }
