/// Re-resolves unresolved NameResolution nodes in package item ASTs.
///
/// When items are added incrementally (e.g., type A references type B, but B
/// doesn't exist yet), A's AST contains NameResolution.Error NotFound for B.
/// After B is added, this module walks A's AST and resolves those errors
/// using the current PackageManager state.
///
/// Parallel structure to AstTransformer.fs, but instead of replacing hashes,
/// it resolves Error names to Ok via PM lookups.
module LibDB.DeferredResolver

open Prelude
open LibExecution.ProgramTypes

module PT = LibExecution.ProgramTypes
module PackageItem = LibDB.PackageItem


// --------------------------------------------------------------------------
// Name resolution helpers (shared logic in NameLookup.fs)
// --------------------------------------------------------------------------

type GenericName = NameLookup.GenericName

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

/// Re-resolve a NameResolution against the current PM state. Already-resolved refs are LEFT PINNED (see the
/// match arm below); only genuinely-unresolved / location-less refs fall back to candidate lookup.
let private reResolveNameResolution
  (contextModules : List<string>)
  (nr : PT.NameResolution<'a>)
  (findInPM : PT.PackageLocation -> Option<Hash>)
  (makePackage : Hash -> 'a)
  (parseName : string -> Result<string * int, string>)
  : PT.NameResolution<'a> =
  match nr.resolved with
  | Error PT.NameResolutionError.InvalidName -> nr

  // Already resolved with a location: LEAVE IT PINNED. Refreshing it to the location's current hash here
  // would be an implicit, whole-store propagation that repoints every dependent on every author.
  // Propagation belongs to autoPropagate (explicit, targeted PropagateUpdate ops from the CLI update
  // path), so re-resolution's only job is resolving genuinely-UNRESOLVED refs (forward-refs). Holding
  // resolved refs stable is also what lets WipRefresh's gate fire on a plain update
  // (reResolvedOps = compactedOps), skipping the redundant whole-store SCC rehash.
  | Ok { location = Some _ } -> nr

  | _ ->
    match List.splitLast nr.originalName with
    | None -> nr
    | Some(modules, lastName) ->
      match parseName lastName with
      | Error _ -> nr
      | Ok(name, version) ->
        let genericName : GenericName =
          { modules = modules; name = name; version = version }

        let result =
          NameLookup.findFirstPackageMatch contextModules genericName (fun loc ->
            findInPM loc)

        match result with
        | Some(hash, loc) ->
          { nr with resolved = Ok { name = makePackage hash; location = Some loc } }
        | None -> nr


// --------------------------------------------------------------------------
// AST walkers
// --------------------------------------------------------------------------

let private reResolveTypeName
  (contextModules : List<string>)
  (findType : PT.PackageLocation -> Option<Hash>)
  (nr : PT.NameResolution<PT.FQTypeName.FQTypeName>)
  : PT.NameResolution<PT.FQTypeName.FQTypeName> =
  reResolveNameResolution
    contextModules
    nr
    findType
    PT.FQTypeName.Package
    parseTypeName


let private reResolveFnName
  (contextModules : List<string>)
  (findFn : PT.PackageLocation -> Option<Hash>)
  (nr : PT.NameResolution<PT.FQFnName.FQFnName>)
  : PT.NameResolution<PT.FQFnName.FQFnName> =
  reResolveNameResolution
    contextModules
    nr
    findFn
    PT.FQFnName.Package
    parseFnOrValueName


let private reResolveValueName
  (contextModules : List<string>)
  (findValue : PT.PackageLocation -> Option<Hash>)
  (nr : PT.NameResolution<PT.FQValueName.FQValueName>)
  : PT.NameResolution<PT.FQValueName.FQValueName> =
  reResolveNameResolution
    contextModules
    nr
    findValue
    PT.FQValueName.Package
    parseFnOrValueName


let private reResolveTraitName
  (contextModules : List<string>)
  (findTrait : PT.PackageLocation -> Option<Hash>)
  (nr : PT.NameResolution<PT.FQTraitName.FQTraitName>)
  : PT.NameResolution<PT.FQTraitName.FQTraitName> =
  reResolveNameResolution
    contextModules
    nr
    findTrait
    PT.FQTraitName.Package
    parseTypeName


// -- TypeReference walker --

let rec private reResolveTypeRef
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (typeRef : PT.TypeReference)
  : PT.TypeReference =
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
  | PT.TBigInt
  | PT.TFloat64
  | PT.TFloat32
  | PT.TDecimal
  | PT.TChar
  | PT.TString
  | PT.TUuid
  | PT.TDateTime
  | PT.TBytes
  | PT.TVariable _ -> typeRef

  | PT.TStream inner ->
    let inner = reResolveTypeRef contextModules pm inner
    PT.TStream inner

  | PT.TList inner ->
    let inner = reResolveTypeRef contextModules pm inner
    PT.TList inner

  | PT.TDict(key, value) ->
    let key = reResolveTypeRef contextModules pm key
    let value = reResolveTypeRef contextModules pm value
    PT.TDict(key, value)

  | PT.TTuple(first, second, rest) ->
    let first = reResolveTypeRef contextModules pm first
    let second = reResolveTypeRef contextModules pm second
    let rest = List.map (reResolveTypeRef contextModules pm) rest
    PT.TTuple(first, second, rest)

  | PT.TCustomType(nr, typeArgs) ->
    let nr = reResolveTypeName contextModules pm.findType nr
    let typeArgs = List.map (reResolveTypeRef contextModules pm) typeArgs
    PT.TCustomType(nr, typeArgs)

  | PT.TFn(args, ret) ->
    let args = NEList.map (reResolveTypeRef contextModules pm) args
    let ret = reResolveTypeRef contextModules pm ret
    PT.TFn(args, ret)


// -- StringSegment walker --

let rec private reResolveStringSegment
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (segment : PT.StringSegment)
  : PT.StringSegment =
  match segment with
  | PT.StringText _ -> segment
  | PT.StringInterpolation expr ->
    let expr = reResolveExpr contextModules pm expr
    PT.StringInterpolation expr


// -- MatchCase walker --

and private reResolveMatchCase
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (case : PT.MatchCase)
  : PT.MatchCase =
  let whenCondition =
    match case.whenCondition with
    | Some expr ->
      let e = reResolveExpr contextModules pm expr
      Some e
    | None -> None

  let rhs = reResolveExpr contextModules pm case.rhs
  { pat = case.pat; whenCondition = whenCondition; rhs = rhs }


// -- PipeExpr walker --

and private reResolvePipeExpr
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (pipeExpr : PT.PipeExpr)
  : PT.PipeExpr =
  match pipeExpr with
  | PT.EPipeLambda(id, pats, body) ->
    let body = reResolveExpr contextModules pm body
    PT.EPipeLambda(id, pats, body)

  | PT.EPipeInfix(id, infix, rhs, implFn) ->
    let rhs = reResolveExpr contextModules pm rhs
    PT.EPipeInfix(id, infix, rhs, implFn)

  | PT.EPipeFnCall(id, nr, typeArgs, args, boundImpls) ->
    let nr = reResolveFnName contextModules pm.findFn nr
    let typeArgs = List.map (reResolveTypeRef contextModules pm) typeArgs
    let args = List.map (reResolveExpr contextModules pm) args
    // Carried, for the reason docs/internals.md gives about `EFnName`: every authoring through
    // `WipRefresh` comes down here, and dropping it re-hashes the item without its bounds.
    PT.EPipeFnCall(id, nr, typeArgs, args, boundImpls)

  | PT.EPipeEnum(id, nr, caseName, fields) ->
    let nr = reResolveTypeName contextModules pm.findType nr
    let fields = List.map (reResolveExpr contextModules pm) fields
    PT.EPipeEnum(id, nr, caseName, fields)

  | PT.EPipeVariable(id, varName, args) ->
    let args = List.map (reResolveExpr contextModules pm) args
    PT.EPipeVariable(id, varName, args)


// -- Expr walker --

and private reResolveExpr
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (expr : PT.Expr)
  : PT.Expr =
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
  | PT.EBigInt _
  | PT.EFloat64 _
  | PT.EFloat32 _
  | PT.EDecimal _
  | PT.EChar _
  | PT.EVariable _
  | PT.EArg _
  | PT.ESelf _ -> expr

  | PT.EString(id, segments) ->
    let segments = List.map (reResolveStringSegment contextModules pm) segments
    PT.EString(id, segments)

  | PT.EIf(id, cond, thenExpr, elseExpr) ->
    let cond = reResolveExpr contextModules pm cond
    let thenExpr = reResolveExpr contextModules pm thenExpr
    let elseExpr =
      match elseExpr with
      | Some e ->
        let e = reResolveExpr contextModules pm e
        Some e
      | None -> None
    PT.EIf(id, cond, thenExpr, elseExpr)

  | PT.EPipe(id, lhs, parts) ->
    let lhs = reResolveExpr contextModules pm lhs
    let parts = List.map (reResolvePipeExpr contextModules pm) parts
    PT.EPipe(id, lhs, parts)

  | PT.EMatch(id, arg, cases) ->
    let arg = reResolveExpr contextModules pm arg
    let cases = List.map (reResolveMatchCase contextModules pm) cases
    PT.EMatch(id, arg, cases)

  | PT.ELet(id, pat, value, body) ->
    let value = reResolveExpr contextModules pm value
    let body = reResolveExpr contextModules pm body
    PT.ELet(id, pat, value, body)

  | PT.EList(id, items) ->
    let items = List.map (reResolveExpr contextModules pm) items
    PT.EList(id, items)

  | PT.EDict(id, pairs) ->
    let pairs =
      List.map
        (fun (k, v) ->
          let v = reResolveExpr contextModules pm v
          (k, v))
        pairs
    PT.EDict(id, pairs)

  | PT.ETuple(id, first, second, rest) ->
    let first = reResolveExpr contextModules pm first
    let second = reResolveExpr contextModules pm second
    let rest = List.map (reResolveExpr contextModules pm) rest
    PT.ETuple(id, first, second, rest)

  | PT.EApply(id, fnExpr, typeArgs, args) ->
    let fnExpr = reResolveExpr contextModules pm fnExpr
    let typeArgs = List.map (reResolveTypeRef contextModules pm) typeArgs
    let args = NEList.map (reResolveExpr contextModules pm) args
    PT.EApply(id, fnExpr, typeArgs, args)

  | PT.EFnName(id, nr, boundImpls, typeArgs) ->
    let nr = reResolveFnName contextModules pm.findFn nr
    let typeArgs = List.map (reResolveTypeRef contextModules pm) typeArgs
    // Carry the bounds the CALL worked out. Re-resolution is about names that could not be
    // resolved at parse time; what this call decided for the callee's bounds was decided
    // once, by the checker, and dropping it here silently un-decides it -- the item is
    // re-hashed without it and every bounded call falls back to resolving at run time.
    PT.EFnName(id, nr, boundImpls, typeArgs)

  | PT.ELambda(id, pats, body) ->
    let body = reResolveExpr contextModules pm body
    PT.ELambda(id, pats, body)

  | PT.EInfix(id, infix, lhs, rhs, implFn) ->
    let lhs = reResolveExpr contextModules pm lhs
    let rhs = reResolveExpr contextModules pm rhs
    PT.EInfix(id, infix, lhs, rhs, implFn)

  | PT.ERecord(id, nr, typeArgs, fields) ->
    let nr = reResolveTypeName contextModules pm.findType nr
    let typeArgs = List.map (reResolveTypeRef contextModules pm) typeArgs
    let fields =
      List.map
        (fun (name, expr) ->
          let expr = reResolveExpr contextModules pm expr
          (name, expr))
        fields
    PT.ERecord(id, nr, typeArgs, fields)

  | PT.ERecordFieldAccess(id, record, fieldName) ->
    let record = reResolveExpr contextModules pm record
    PT.ERecordFieldAccess(id, record, fieldName)

  | PT.ERecordUpdate(id, record, updates) ->
    let record = reResolveExpr contextModules pm record
    let updates =
      NEList.map
        (fun (name, expr) ->
          let expr = reResolveExpr contextModules pm expr
          (name, expr))
        updates
    PT.ERecordUpdate(id, record, updates)

  | PT.EEnum(id, nr, typeArgs, caseName, fields) ->
    let nr = reResolveTypeName contextModules pm.findType nr
    let typeArgs = List.map (reResolveTypeRef contextModules pm) typeArgs
    let fields = List.map (reResolveExpr contextModules pm) fields
    PT.EEnum(id, nr, typeArgs, caseName, fields)

  | PT.EValue(id, nr) ->
    let nr = reResolveValueName contextModules pm.findValue nr
    match nr.resolved with
    | Error PT.NameResolutionError.NotFound ->
      // A qualified fn used in value position (`let f = Mod.fn`) lowers to EValue
      // when NEITHER a value nor a fn resolves at parse time (value-first default).
      // Once the fn lands, refreshing only the value namespace misses it — so retry
      // the SAME name as a fn and promote to EFnName if it now resolves.
      let fnNr : PT.NameResolution<PT.FQFnName.FQFnName> =
        { originalName = nr.originalName
          resolved = Error PT.NameResolutionError.NotFound }
      let fnNr = reResolveFnName contextModules pm.findFn fnNr
      match fnNr.resolved with
      | Ok _ -> PT.EFnName(id, fnNr, [], [])
      | Error _ -> PT.EValue(id, nr)
    | _ -> PT.EValue(id, nr)

  | PT.EUnwrap(id, operand) ->
    let operand = reResolveExpr contextModules pm operand
    PT.EUnwrap(id, operand)
  | PT.EStatement(id, first, next) ->
    let first = reResolveExpr contextModules pm first
    let next = reResolveExpr contextModules pm next
    PT.EStatement(id, first, next)
  | PT.EExtern(id, library, symbol, args) ->
    PT.EExtern(id, library, symbol, List.map (reResolveExpr contextModules pm) args)


// -- TypeDeclaration walker --

let private reResolveTypeDefinition
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (def : PT.TypeDeclaration.Definition)
  : PT.TypeDeclaration.Definition =
  match def with
  | PT.TypeDeclaration.Alias typeRef ->
    let typeRef = reResolveTypeRef contextModules pm typeRef
    PT.TypeDeclaration.Alias typeRef

  | PT.TypeDeclaration.Record fields ->
    let fields =
      NEList.map
        (fun (f : PT.TypeDeclaration.RecordField) ->
          let typ = reResolveTypeRef contextModules pm f.typ
          { f with typ = typ })
        fields
    PT.TypeDeclaration.Record fields

  | PT.TypeDeclaration.Enum cases ->
    let cases =
      NEList.map
        (fun (c : PT.TypeDeclaration.EnumCase) ->
          let fields =
            List.map
              (fun (f : PT.TypeDeclaration.EnumField) ->
                let typ = reResolveTypeRef contextModules pm f.typ
                { f with typ = typ })
              c.fields
          { c with fields = fields })
        cases
    PT.TypeDeclaration.Enum cases


// --------------------------------------------------------------------------
// Public API
// --------------------------------------------------------------------------

/// Re-resolve all unresolved NameResolutions in a PackageType
let private reResolveTraitRef
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (r : PT.TraitRef)
  : PT.TraitRef =
  let trait_ = reResolveTraitName contextModules pm.findTrait r.trait_
  let typeArgs = List.map (reResolveTypeRef contextModules pm) r.typeArgs
  { trait_ = trait_; typeArgs = typeArgs }

/// The parameter walk `reResolveFn` and `reResolveTrait` share.
let private reResolveParams
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (ps : NEList<PT.PackageFn.Parameter>)
  : NEList<PT.PackageFn.Parameter> =
  ps
  |> NEList.map (fun (p : PT.PackageFn.Parameter) ->
    let typ = reResolveTypeRef contextModules pm p.typ
    { p with typ = typ })


let private reResolveBounds
  (contextModules : List<string>)
  (pm : PT.PackageManager)
  (bounds : List<PT.Bound>)
  : List<PT.Bound> =
  bounds
  |> List.map (fun (b : PT.Bound) ->
    let trait_ = reResolveTraitRef contextModules pm b.trait_
    { b with trait_ = trait_ })

let reResolveType
  (pm : PT.PackageManager)
  (owner : string)
  (modules : List<string>)
  (t : PT.PackageType.PackageType)
  : PT.PackageType.PackageType =
  let contextModules = owner :: modules

  let definition = reResolveTypeDefinition contextModules pm t.declaration.definition
  let bounds = reResolveBounds contextModules pm t.declaration.bounds

  { t with
      declaration = { t.declaration with definition = definition; bounds = bounds } }


/// Re-resolve all unresolved NameResolutions in a PackageFn
let reResolveFn
  (pm : PT.PackageManager)
  (owner : string)
  (modules : List<string>)
  (f : PT.PackageFn.PackageFn)
  : PT.PackageFn.PackageFn =
  let contextModules = owner :: modules

  let body = reResolveExpr contextModules pm f.body

  let parameters = reResolveParams contextModules pm f.parameters

  let returnType = reResolveTypeRef contextModules pm f.returnType
  let bounds = reResolveBounds contextModules pm f.bounds

  { f with
      body = body
      parameters = parameters
      returnType = returnType
      bounds = bounds }


/// Re-resolve a trait's bounds and method signatures
let reResolveTrait
  (pm : PT.PackageManager)
  (owner : string)
  (modules : List<string>)
  (t : PT.Trait.Trait)
  : PT.Trait.Trait =
  let contextModules = owner :: modules

  let bounds = reResolveBounds contextModules pm t.bounds
  let methods =
    NEList.map
      (fun (m : PT.Trait.Method) ->
        let parameters = reResolveParams contextModules pm m.parameters
        let returnType = reResolveTypeRef contextModules pm m.returnType
        // A method's OWN bounds are hashed and read by the checker like the trait's,
        // so a forward-referenced trait in one has to be repaired here too.
        let methodBounds = reResolveBounds contextModules pm m.bounds
        { m with
            parameters = parameters
            returnType = returnType
            bounds = methodBounds })
      t.methods
  { t with bounds = bounds; methods = methods }


/// Re-resolve an impl's trait, self type, bounds and method targets. The
/// context is the impl's own module (its member path), which is where a
/// method declared in the block resolves from.
let reResolveImpl
  (pm : PT.PackageManager)
  (owner : string)
  (modules : List<string>)
  (name : string)
  (i : PT.TraitImpl.TraitImpl)
  : PT.TraitImpl.TraitImpl =
  let contextModules = owner :: modules @ [ name ]

  let trait_ = reResolveTraitName contextModules pm.findTrait i.trait_
  let traitTypeArgs = List.map (reResolveTypeRef contextModules pm) i.traitTypeArgs
  let self = reResolveTypeRef contextModules pm i.self
  let bounds = reResolveBounds contextModules pm i.bounds
  let methods =
    i.methods
    |> List.map (fun (m, nr) ->
      let nr = reResolveFnName contextModules pm.findFn nr
      (m, nr))
  { i with
      trait_ = trait_
      traitTypeArgs = traitTypeArgs
      self = self
      bounds = bounds
      methods = methods }


/// Re-resolve all unresolved NameResolutions in a PackageValue
let reResolveValue
  (pm : PT.PackageManager)
  (owner : string)
  (modules : List<string>)
  (v : PT.PackageValue.PackageValue)
  : PT.PackageValue.PackageValue =
  let contextModules = owner :: modules

  let body = reResolveExpr contextModules pm v.body
  { v with body = body }


/// Re-resolve all unresolved names in a PackageTest body.
let reResolveTest
  (pm : PT.PackageManager)
  (owner : string)
  (modules : List<string>)
  (test : PT.PackageTest.PackageTest)
  : PT.PackageTest.PackageTest =
  let body = reResolveExpr (owner :: modules) pm test.body
  { test with body = body }
