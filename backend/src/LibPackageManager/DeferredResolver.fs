/// Re-resolves unresolved NameResolution nodes in package item ASTs.
///
/// When items are added incrementally (e.g., type A references type B, but B
/// doesn't exist yet), A's AST contains NameResolution.Error NotFound for B.
/// After B is added, this module walks A's AST and resolves those errors
/// using the current PackageManager state.
///
/// Parallel structure to AstTransformer.fs, but instead of replacing hashes,
/// it resolves Error names to Ok via PM lookups.
module LibPackageManager.DeferredResolver

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes


// --------------------------------------------------------------------------
// Name resolution helpers (shared logic in NameLookup.fs)
// --------------------------------------------------------------------------

type GenericName = NameLookup.GenericName

let private namesToTry = NameLookup.namesToTry

let private typeNameRegex =
  System.Text.RegularExpressions.Regex(
    @"^[A-Z][a-z0-9A-Z_']*$",
    System.Text.RegularExpressions.RegexOptions.Compiled
  )

let private fnVersionedRegex =
  System.Text.RegularExpressions.Regex(
    @"^([a-z][a-z0-9A-Z_']*?)_v(\d+)$",
    System.Text.RegularExpressions.RegexOptions.Compiled
  )

let private fnUnversionedRegex =
  System.Text.RegularExpressions.Regex(
    @"^([a-z][a-z0-9A-Z_']*?)$",
    System.Text.RegularExpressions.RegexOptions.Compiled
  )


/// Parse a type name: just the name, version is always 0
let private parseTypeName (name : string) : Result<string * int, string> =
  if typeNameRegex.IsMatch(name) then Ok(name, 0) else Error "Bad type name"


/// Parse a fn/value name: extract optional _vN suffix
let private parseFnOrValueName (name : string) : Result<string * int, string> =
  let m = fnVersionedRegex.Match(name)

  if m.Success then
    Ok(m.Groups[1].Value, int m.Groups[2].Value)
  else
    let m2 = fnUnversionedRegex.Match(name)
    if m2.Success then Ok(m2.Groups[1].Value, 0) else Error "Bad fn/value name"


// --------------------------------------------------------------------------
// Core re-resolution
// --------------------------------------------------------------------------

/// Try to re-resolve a single NameResolution that is Error NotFound.
let private reResolveNameResolution
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (nr : PT.NameResolution<'a>)
  (findInPM : (PT.BranchId * PT.PackageLocation) -> Ply<Option<Hash>>)
  (makePackage : Hash -> 'a)
  (parseName : string -> Result<string * int, string>)
  : Ply<PT.NameResolution<'a>> =
  match nr.resolved with
  | Ok _ -> Ply nr
  | Error PT.NameResolutionError.InvalidName -> Ply nr
  | Error PT.NameResolutionError.NotFound ->
    match List.splitLast nr.originalName with
    | None -> Ply nr
    | Some(modules, lastName) ->
      uply {
        match parseName lastName with
        | Error _ -> return nr
        | Ok(name, version) ->
          let genericName : GenericName =
            { modules = modules; name = name; version = version }

          let candidates = namesToTry contextModules genericName

          let! result =
            Ply.List.foldSequentially
              (fun acc (candidate : GenericName) ->
                match acc with
                | Some _ -> Ply acc
                | None ->
                  uply {
                    match candidate.modules with
                    | [] -> return None
                    | owner :: mods ->
                      let loc : PT.PackageLocation =
                        { owner = owner; modules = mods; name = candidate.name }

                      match! findInPM (branchId, loc) with
                      | Some hash -> return Some hash
                      | None -> return None
                  })
              None
              candidates

          match result with
          | Some hash -> return { nr with resolved = Ok(makePackage hash) }
          | None -> return nr
      }


// --------------------------------------------------------------------------
// AST walkers
// --------------------------------------------------------------------------

let private reResolveTypeName
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (findType : (PT.BranchId * PT.PackageLocation) -> Ply<Option<Hash>>)
  (nr : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  : Ply<PT.NameResolution<PT.FQTypeName.FQTypeName>> =
  reResolveNameResolution
    branchId
    contextModules
    nr
    findType
    PT.FQTypeName.Package
    parseTypeName


let private reResolveFnName
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (findFn : (PT.BranchId * PT.PackageLocation) -> Ply<Option<Hash>>)
  (nr : PT.NameResolution<PT.FQFnName.FQFnName>)
  : Ply<PT.NameResolution<PT.FQFnName.FQFnName>> =
  reResolveNameResolution
    branchId
    contextModules
    nr
    findFn
    PT.FQFnName.Package
    parseFnOrValueName


let private reResolveValueName
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (findValue : (PT.BranchId * PT.PackageLocation) -> Ply<Option<Hash>>)
  (nr : PT.NameResolution<PT.FQValueName.FQValueName>)
  : Ply<PT.NameResolution<PT.FQValueName.FQValueName>> =
  reResolveNameResolution
    branchId
    contextModules
    nr
    findValue
    PT.FQValueName.Package
    parseFnOrValueName


// -- TypeReference walker --

let rec private reResolveTypeRef
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (typeRef : PT.TypeReference)
  : Ply<PT.TypeReference> =
  uply {
    match typeRef with
    | PT.TUnit
    | PT.TBool
    | PT.TInt8
    | PT.TUInt8
    | PT.TInt16
    | PT.TUInt16
    | PT.TInt32
    | PT.TUInt32
    | PT.TInt64
    | PT.TUInt64
    | PT.TInt128
    | PT.TUInt128
    | PT.TFloat
    | PT.TChar
    | PT.TString
    | PT.TUuid
    | PT.TDateTime
    | PT.TBlob
    | PT.TVariable _ -> return typeRef

    | PT.TStream inner ->
      let! inner = reResolveTypeRef branchId contextModules pm inner
      return PT.TStream inner

    | PT.TList inner ->
      let! inner = reResolveTypeRef branchId contextModules pm inner
      return PT.TList inner

    | PT.TDict inner ->
      let! inner = reResolveTypeRef branchId contextModules pm inner
      return PT.TDict inner

    | PT.TDB inner ->
      let! inner = reResolveTypeRef branchId contextModules pm inner
      return PT.TDB inner

    | PT.TTuple(first, second, rest) ->
      let! first = reResolveTypeRef branchId contextModules pm first
      let! second = reResolveTypeRef branchId contextModules pm second
      let! rest =
        Ply.List.mapSequentially (reResolveTypeRef branchId contextModules pm) rest
      return PT.TTuple(first, second, rest)

    | PT.TCustomType(nr, typeArgs) ->
      let! nr = reResolveTypeName branchId contextModules pm.findType nr
      let! typeArgs =
        Ply.List.mapSequentially
          (reResolveTypeRef branchId contextModules pm)
          typeArgs
      return PT.TCustomType(nr, typeArgs)

    | PT.TFn(args, ret) ->
      let! args =
        Ply.NEList.mapSequentially (reResolveTypeRef branchId contextModules pm) args
      let! ret = reResolveTypeRef branchId contextModules pm ret
      return PT.TFn(args, ret)
  }


// -- StringSegment walker --

let rec private reResolveStringSegment
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (segment : PT.StringSegment)
  : Ply<PT.StringSegment> =
  uply {
    match segment with
    | PT.StringText _ -> return segment
    | PT.StringInterpolation expr ->
      let! expr = reResolveExpr branchId contextModules pm expr
      return PT.StringInterpolation expr
  }


// -- MatchCase walker --

and private reResolveMatchCase
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (case : PT.MatchCase)
  : Ply<PT.MatchCase> =
  uply {
    let! whenCondition =
      match case.whenCondition with
      | Some expr ->
        uply {
          let! e = reResolveExpr branchId contextModules pm expr
          return Some e
        }
      | None -> Ply None

    let! rhs = reResolveExpr branchId contextModules pm case.rhs
    return { pat = case.pat; whenCondition = whenCondition; rhs = rhs }
  }


// -- PipeExpr walker --

and private reResolvePipeExpr
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (pipeExpr : PT.PipeExpr)
  : Ply<PT.PipeExpr> =
  uply {
    match pipeExpr with
    | PT.EPipeLambda(id, pats, body) ->
      let! body = reResolveExpr branchId contextModules pm body
      return PT.EPipeLambda(id, pats, body)

    | PT.EPipeInfix(id, infix, rhs) ->
      let! rhs = reResolveExpr branchId contextModules pm rhs
      return PT.EPipeInfix(id, infix, rhs)

    | PT.EPipeFnCall(id, nr, typeArgs, args) ->
      let! nr = reResolveFnName branchId contextModules pm.findFn nr
      let! typeArgs =
        Ply.List.mapSequentially
          (reResolveTypeRef branchId contextModules pm)
          typeArgs
      let! args =
        Ply.List.mapSequentially (reResolveExpr branchId contextModules pm) args
      return PT.EPipeFnCall(id, nr, typeArgs, args)

    | PT.EPipeEnum(id, nr, caseName, fields) ->
      let! nr = reResolveTypeName branchId contextModules pm.findType nr
      let! fields =
        Ply.List.mapSequentially (reResolveExpr branchId contextModules pm) fields
      return PT.EPipeEnum(id, nr, caseName, fields)

    | PT.EPipeVariable(id, varName, args) ->
      let! args =
        Ply.List.mapSequentially (reResolveExpr branchId contextModules pm) args
      return PT.EPipeVariable(id, varName, args)
  }


// -- Expr walker --

and private reResolveExpr
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (expr : PT.Expr)
  : Ply<PT.Expr> =
  uply {
    match expr with
    | PT.EUnit _
    | PT.EBool _
    | PT.EInt8 _
    | PT.EUInt8 _
    | PT.EInt16 _
    | PT.EUInt16 _
    | PT.EInt32 _
    | PT.EUInt32 _
    | PT.EInt64 _
    | PT.EUInt64 _
    | PT.EInt128 _
    | PT.EUInt128 _
    | PT.EFloat _
    | PT.EChar _
    | PT.EVariable _
    | PT.EArg _
    | PT.ESelf _ -> return expr

    | PT.EString(id, segments) ->
      let! segments =
        Ply.List.mapSequentially
          (reResolveStringSegment branchId contextModules pm)
          segments
      return PT.EString(id, segments)

    | PT.EIf(id, cond, thenExpr, elseExpr) ->
      let! cond = reResolveExpr branchId contextModules pm cond
      let! thenExpr = reResolveExpr branchId contextModules pm thenExpr
      let! elseExpr =
        match elseExpr with
        | Some e ->
          uply {
            let! e = reResolveExpr branchId contextModules pm e
            return Some e
          }
        | None -> Ply None
      return PT.EIf(id, cond, thenExpr, elseExpr)

    | PT.EPipe(id, lhs, parts) ->
      let! lhs = reResolveExpr branchId contextModules pm lhs
      let! parts =
        Ply.List.mapSequentially (reResolvePipeExpr branchId contextModules pm) parts
      return PT.EPipe(id, lhs, parts)

    | PT.EMatch(id, arg, cases) ->
      let! arg = reResolveExpr branchId contextModules pm arg
      let! cases =
        Ply.List.mapSequentially
          (reResolveMatchCase branchId contextModules pm)
          cases
      return PT.EMatch(id, arg, cases)

    | PT.ELet(id, pat, value, body) ->
      let! value = reResolveExpr branchId contextModules pm value
      let! body = reResolveExpr branchId contextModules pm body
      return PT.ELet(id, pat, value, body)

    | PT.EList(id, items) ->
      let! items =
        Ply.List.mapSequentially (reResolveExpr branchId contextModules pm) items
      return PT.EList(id, items)

    | PT.EDict(id, pairs) ->
      let! pairs =
        Ply.List.mapSequentially
          (fun (k, v) ->
            uply {
              let! v = reResolveExpr branchId contextModules pm v
              return (k, v)
            })
          pairs
      return PT.EDict(id, pairs)

    | PT.ETuple(id, first, second, rest) ->
      let! first = reResolveExpr branchId contextModules pm first
      let! second = reResolveExpr branchId contextModules pm second
      let! rest =
        Ply.List.mapSequentially (reResolveExpr branchId contextModules pm) rest
      return PT.ETuple(id, first, second, rest)

    | PT.EApply(id, fnExpr, typeArgs, args) ->
      let! fnExpr = reResolveExpr branchId contextModules pm fnExpr
      let! typeArgs =
        Ply.List.mapSequentially
          (reResolveTypeRef branchId contextModules pm)
          typeArgs
      let! args =
        Ply.NEList.mapSequentially (reResolveExpr branchId contextModules pm) args
      return PT.EApply(id, fnExpr, typeArgs, args)

    | PT.EFnName(id, nr) ->
      let! nr = reResolveFnName branchId contextModules pm.findFn nr
      return PT.EFnName(id, nr)

    | PT.ELambda(id, pats, body) ->
      let! body = reResolveExpr branchId contextModules pm body
      return PT.ELambda(id, pats, body)

    | PT.EInfix(id, infix, lhs, rhs) ->
      let! lhs = reResolveExpr branchId contextModules pm lhs
      let! rhs = reResolveExpr branchId contextModules pm rhs
      return PT.EInfix(id, infix, lhs, rhs)

    | PT.ERecord(id, nr, typeArgs, fields) ->
      let! nr = reResolveTypeName branchId contextModules pm.findType nr
      let! typeArgs =
        Ply.List.mapSequentially
          (reResolveTypeRef branchId contextModules pm)
          typeArgs
      let! fields =
        Ply.List.mapSequentially
          (fun (name, expr) ->
            uply {
              let! expr = reResolveExpr branchId contextModules pm expr
              return (name, expr)
            })
          fields
      return PT.ERecord(id, nr, typeArgs, fields)

    | PT.ERecordFieldAccess(id, record, fieldName) ->
      let! record = reResolveExpr branchId contextModules pm record
      return PT.ERecordFieldAccess(id, record, fieldName)

    | PT.ERecordUpdate(id, record, updates) ->
      let! record = reResolveExpr branchId contextModules pm record
      let! updates =
        Ply.NEList.mapSequentially
          (fun (name, expr) ->
            uply {
              let! expr = reResolveExpr branchId contextModules pm expr
              return (name, expr)
            })
          updates
      return PT.ERecordUpdate(id, record, updates)

    | PT.EEnum(id, nr, typeArgs, caseName, fields) ->
      let! nr = reResolveTypeName branchId contextModules pm.findType nr
      let! typeArgs =
        Ply.List.mapSequentially
          (reResolveTypeRef branchId contextModules pm)
          typeArgs
      let! fields =
        Ply.List.mapSequentially (reResolveExpr branchId contextModules pm) fields
      return PT.EEnum(id, nr, typeArgs, caseName, fields)

    | PT.EValue(id, nr) ->
      let! nr = reResolveValueName branchId contextModules pm.findValue nr
      return PT.EValue(id, nr)

    | PT.EStatement(id, first, next) ->
      let! first = reResolveExpr branchId contextModules pm first
      let! next = reResolveExpr branchId contextModules pm next
      return PT.EStatement(id, first, next)
  }


// -- TypeDeclaration walker --

let private reResolveTypeDefinition
  (branchId : PT.BranchId)
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (def : PT.TypeDeclaration.Definition)
  : Ply<PT.TypeDeclaration.Definition> =
  uply {
    match def with
    | PT.TypeDeclaration.Alias typeRef ->
      let! typeRef = reResolveTypeRef branchId contextModules pm typeRef
      return PT.TypeDeclaration.Alias typeRef

    | PT.TypeDeclaration.Record fields ->
      let! fields =
        Ply.NEList.mapSequentially
          (fun (f : PT.TypeDeclaration.RecordField) ->
            uply {
              let! typ = reResolveTypeRef branchId contextModules pm f.typ
              return { f with typ = typ }
            })
          fields
      return PT.TypeDeclaration.Record fields

    | PT.TypeDeclaration.Enum cases ->
      let! cases =
        Ply.NEList.mapSequentially
          (fun (c : PT.TypeDeclaration.EnumCase) ->
            uply {
              let! fields =
                Ply.List.mapSequentially
                  (fun (f : PT.TypeDeclaration.EnumField) ->
                    uply {
                      let! typ = reResolveTypeRef branchId contextModules pm f.typ
                      return { f with typ = typ }
                    })
                  c.fields
              return { c with fields = fields }
            })
          cases
      return PT.TypeDeclaration.Enum cases
  }


// --------------------------------------------------------------------------
// Public API
// --------------------------------------------------------------------------

/// Re-resolve all unresolved NameResolutions in a PackageType
let reResolveType
  (pm : PT.PackageManager)
  (branchId : PT.BranchId)
  (owner : string)
  (modules : List<string>)
  (t : PT.PackageType.PackageType)
  : Ply<PT.PackageType.PackageType> =
  let contextModules = owner :: modules

  uply {
    let! definition =
      reResolveTypeDefinition branchId contextModules pm t.declaration.definition

    return { t with declaration = { t.declaration with definition = definition } }
  }


/// Re-resolve all unresolved NameResolutions in a PackageFn
let reResolveFn
  (pm : PT.PackageManager)
  (branchId : PT.BranchId)
  (owner : string)
  (modules : List<string>)
  (f : PT.PackageFn.PackageFn)
  : Ply<PT.PackageFn.PackageFn> =
  let contextModules = owner :: modules

  uply {
    let! body = reResolveExpr branchId contextModules pm f.body

    let! parameters =
      Ply.NEList.mapSequentially
        (fun (p : PT.PackageFn.Parameter) ->
          uply {
            let! typ = reResolveTypeRef branchId contextModules pm p.typ
            return { p with typ = typ }
          })
        f.parameters

    let! returnType = reResolveTypeRef branchId contextModules pm f.returnType

    return { f with body = body; parameters = parameters; returnType = returnType }
  }


/// Re-resolve all unresolved NameResolutions in a PackageValue
let reResolveValue
  (pm : PT.PackageManager)
  (branchId : PT.BranchId)
  (owner : string)
  (modules : List<string>)
  (v : PT.PackageValue.PackageValue)
  : Ply<PT.PackageValue.PackageValue> =
  let contextModules = owner :: modules

  uply {
    let! body = reResolveExpr branchId contextModules pm v.body
    return { v with body = body }
  }
