// IRPrinter.fs - Shared IR pretty printers
//
// Provides pinned formatting for ANF, MIR, LIR, and symbolic LIR output.

module IRPrinter

open ANF
open MIR
open LIR

/// Pretty-print ANF atom
let private prettyPrintANFAtom = function
    | ANF.UnitLiteral -> "()"
    | ANF.IntLiteral n -> string (ANF.sizedIntToInt64 n)
    | ANF.BoolLiteral b -> if b then "true" else "false"
    | ANF.StringLiteral s -> $"\"{s}\""
    | ANF.FloatLiteral f -> string f
    | ANF.Var (ANF.TempId n) -> $"t{n}"
    | ANF.FuncRef name -> $"&{name}"

/// Pretty-print ANF binary operator
let private prettyPrintANFOp = function
    | ANF.Add -> "+"
    | ANF.Sub -> "-"
    | ANF.Mul -> "*"
    | ANF.Div -> "/"
    | ANF.Mod -> "%"
    | ANF.Shl -> "<<"
    | ANF.Shr -> ">>"
    | ANF.BitAnd -> "&"
    | ANF.BitOr -> "|"
    | ANF.BitXor -> "^"
    | ANF.Eq -> "=="
    | ANF.Neq -> "!="
    | ANF.Lt -> "<"
    | ANF.Gt -> ">"
    | ANF.Lte -> "<="
    | ANF.Gte -> ">="
    | ANF.And -> "&&"
    | ANF.Or -> "||"

/// Pretty-print ANF unary operator
let private prettyPrintANFUnaryOp = function
    | ANF.Neg -> "-"
    | ANF.Not -> "!"
    | ANF.BitNot -> "~~~"

let private prettyPrintANFRcKind = function
    | ANF.GenericHeap -> "generic"
    | ANF.TaggedList -> "list"

/// Append a type suffix when available
let private appendANFTypeSuffix (typOpt: AST.Type option) (value: string) : string =
    match typOpt with
    | None -> value
    | Some typ -> $"{value} : {typ}"

/// Pretty-print ANF complex expression
let private prettyPrintANFCExpr = function
    | ANF.Atom atom -> prettyPrintANFAtom atom
    | ANF.TypedAtom (atom, typ) -> $"{prettyPrintANFAtom atom} : {typ}"
    | ANF.Prim (op, left, right) ->
        $"{prettyPrintANFAtom left} {prettyPrintANFOp op} {prettyPrintANFAtom right}"
    | ANF.UnaryPrim (op, operand) ->
        $"{prettyPrintANFUnaryOp op}{prettyPrintANFAtom operand}"
    | ANF.Call (funcName, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"{funcName}({argStr})"
    | ANF.BorrowedCall (funcName, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"borrowed {funcName}({argStr})"
    | ANF.IndirectCall (func, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"IndirectCall({prettyPrintANFAtom func}, [{argStr}])"
    | ANF.ClosureAlloc (funcName, captures) ->
        let capsStr = captures |> List.map prettyPrintANFAtom |> String.concat ", "
        $"ClosureAlloc({funcName}, [{capsStr}])"
    | ANF.ClosureCall (closure, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"ClosureCall({prettyPrintANFAtom closure}, [{argStr}])"
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        $"if {prettyPrintANFAtom cond} then {prettyPrintANFAtom thenAtom} else {prettyPrintANFAtom elseAtom}"
    | ANF.TupleAlloc elems ->
        let elemsStr = elems |> List.map prettyPrintANFAtom |> String.concat ", "
        $"({elemsStr})"
    | ANF.TupleGet (tupleAtom, index) ->
        $"{prettyPrintANFAtom tupleAtom}.{index}"
    | ANF.RefCountInc (atom, payloadSize, kind) ->
        $"rc_inc({prettyPrintANFAtom atom}, size={payloadSize}, kind={prettyPrintANFRcKind kind})"
    | ANF.RefCountDec (atom, payloadSize, kind) ->
        $"rc_dec({prettyPrintANFAtom atom}, size={payloadSize}, kind={prettyPrintANFRcKind kind})"
    | ANF.StringConcat (left, right) ->
        $"{prettyPrintANFAtom left} ++ {prettyPrintANFAtom right}"
    | ANF.Print (atom, valueType) ->
        $"print({prettyPrintANFAtom atom}, type={valueType})"
    | ANF.RuntimeError message ->
        $"runtime_error(\"{message}\")"
    | ANF.FileReadText path ->
        $"FileReadText({prettyPrintANFAtom path})"
    | ANF.FileExists path ->
        $"FileExists({prettyPrintANFAtom path})"
    | ANF.FileWriteText (path, content) ->
        $"FileWriteText({prettyPrintANFAtom path}, {prettyPrintANFAtom content})"
    | ANF.FileAppendText (path, content) ->
        $"FileAppendText({prettyPrintANFAtom path}, {prettyPrintANFAtom content})"
    | ANF.FileDelete path ->
        $"FileDelete({prettyPrintANFAtom path})"
    | ANF.FileSetExecutable path ->
        $"FileSetExecutable({prettyPrintANFAtom path})"
    | ANF.FileWriteFromPtr (path, ptr, length) ->
        $"FileWriteFromPtr({prettyPrintANFAtom path}, {prettyPrintANFAtom ptr}, {prettyPrintANFAtom length})"
    | ANF.RawAlloc numBytes ->
        $"RawAlloc({prettyPrintANFAtom numBytes})"
    | ANF.RawFree ptr ->
        $"RawFree({prettyPrintANFAtom ptr})"
    | ANF.RawGet (ptr, byteOffset, valueType) ->
        let baseText = $"RawGet({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset})"
        appendANFTypeSuffix valueType baseText
    | ANF.RawGetByte (ptr, byteOffset) ->
        $"RawGetByte({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset})"
    | ANF.RawSet (ptr, byteOffset, value, valueType) ->
        let baseText = $"RawSet({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset}, {prettyPrintANFAtom value})"
        appendANFTypeSuffix valueType baseText
    | ANF.RawSetByte (ptr, byteOffset, value) ->
        $"RawSetByte({prettyPrintANFAtom ptr}, {prettyPrintANFAtom byteOffset}, {prettyPrintANFAtom value})"
    | ANF.FloatSqrt atom ->
        $"FloatSqrt({prettyPrintANFAtom atom})"
    | ANF.FloatAbs atom ->
        $"FloatAbs({prettyPrintANFAtom atom})"
    | ANF.FloatNeg atom ->
        $"FloatNeg({prettyPrintANFAtom atom})"
    | ANF.Int64ToFloat atom ->
        $"Int64ToFloat({prettyPrintANFAtom atom})"
    | ANF.FloatToInt64 atom ->
        $"FloatToInt64({prettyPrintANFAtom atom})"
    | ANF.FloatToBits atom ->
        $"FloatToBits({prettyPrintANFAtom atom})"
    | ANF.FloatToString atom ->
        $"FloatToString({prettyPrintANFAtom atom})"
    | ANF.RefCountIncString str ->
        $"RefCountIncString({prettyPrintANFAtom str})"
    | ANF.RefCountDecString str ->
        $"RefCountDecString({prettyPrintANFAtom str})"
    | ANF.RandomInt64 ->
        "RandomInt64()"
    | ANF.DateNow ->
        "DateNow()"
    | ANF.TailCall (funcName, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"TailCall({funcName}, [{argStr}])"
    | ANF.IndirectTailCall (func, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"IndirectTailCall({prettyPrintANFAtom func}, [{argStr}])"
    | ANF.ClosureTailCall (closure, args) ->
        let argStr = args |> List.map prettyPrintANFAtom |> String.concat ", "
        $"ClosureTailCall({prettyPrintANFAtom closure}, [{argStr}])"

/// Pretty-print ANF expression
let rec private prettyPrintANFExpr = function
    | ANF.Return atom -> $"return {prettyPrintANFAtom atom}"
    | ANF.Let (var, cexpr, body) ->
        let cexprStr = prettyPrintANFCExpr cexpr
        let bodyStr = prettyPrintANFExpr body
        $"let {var} = {cexprStr}\n{bodyStr}"
    | ANF.If (cond, thenBranch, elseBranch) ->
        let condStr = prettyPrintANFAtom cond
        let thenStr = prettyPrintANFExpr thenBranch
        let elseStr = prettyPrintANFExpr elseBranch
        $"if {condStr} then\n{thenStr}\nelse\n{elseStr}"

/// Format ANF program in a pinned format
let formatANF (ANF.Program (functions, mainExpr)) : string =
    let funcStrs =
        functions
        |> List.map (fun func ->
            $"Function {func.Name}:\n{prettyPrintANFExpr func.Body}")
        |> String.concat "\n\n"

    let mainStr = prettyPrintANFExpr mainExpr

    if List.isEmpty functions then
        mainStr
    else
        funcStrs + "\n\nMain:\n" + mainStr

/// Pretty-print MIR operand
let private prettyPrintMIROperand = function
    | MIR.Int64Const n -> string n
    | MIR.BoolConst b -> if b then "true" else "false"
    | MIR.FloatSymbol value -> $"float[{value}]"
    | MIR.StringSymbol value -> $"str[{value}]"
    | MIR.Register (MIR.VReg n) -> $"v{n}"
    | MIR.FuncAddr name -> $"&{name}"

/// Pretty-print MIR operator
let private prettyPrintMIROp = function
    | MIR.Add -> "+"
    | MIR.Sub -> "-"
    | MIR.Mul -> "*"
    | MIR.Div -> "/"
    | MIR.Mod -> "%"
    | MIR.Shl -> "<<"
    | MIR.Shr -> ">>"
    | MIR.BitAnd -> "&"
    | MIR.BitOr -> "|"
    | MIR.BitXor -> "^"
    | MIR.Eq -> "=="
    | MIR.Neq -> "!="
    | MIR.Lt -> "<"
    | MIR.Gt -> ">"
    | MIR.Lte -> "<="
    | MIR.Gte -> ">="
    | MIR.And -> "&&"
    | MIR.Or -> "||"

/// Pretty-print MIR unary operator
let private prettyPrintMIRUnaryOp = function
    | MIR.Neg -> "-"
    | MIR.Not -> "!"
    | MIR.BitNot -> "~~~"

let private prettyPrintMIRRcKind = function
    | MIR.GenericHeap -> "generic"
    | MIR.TaggedList -> "list"

/// Pretty-print MIR virtual register
let private prettyPrintMIRVReg (MIR.VReg n) : string =
    $"v{n}"

/// Pretty-print MIR label
let private prettyPrintMIRLabel (MIR.Label name) : string =
    name

/// Append a type suffix when available
let private appendTypeSuffix (typOpt: AST.Type option) (value: string) : string =
    match typOpt with
    | None -> value
    | Some typ -> $"{value} : {typ}"

/// Pretty-print MIR instruction
let private prettyPrintMIRInstr (instr: MIR.Instr) : string =
    match instr with
    | MIR.Mov (dest, src, valueType) ->
        let baseText = $"{prettyPrintMIRVReg dest} <- {prettyPrintMIROperand src}"
        appendTypeSuffix valueType baseText
    | MIR.BinOp (dest, op, left, right, operandType) ->
        $"{prettyPrintMIRVReg dest} <- {prettyPrintMIROperand left} {prettyPrintMIROp op} {prettyPrintMIROperand right} : {operandType}"
    | MIR.UnaryOp (dest, op, src) ->
        $"{prettyPrintMIRVReg dest} <- {prettyPrintMIRUnaryOp op}{prettyPrintMIROperand src}"
    | MIR.Call (dest, funcName, args, _, _) ->
        let argStr = args |> List.map prettyPrintMIROperand |> String.concat ", "
        $"{prettyPrintMIRVReg dest} <- Call({funcName}, [{argStr}])"
    | MIR.TailCall (funcName, args, _, _) ->
        let argStr = args |> List.map prettyPrintMIROperand |> String.concat ", "
        $"TailCall({funcName}, [{argStr}])"
    | MIR.IndirectCall (dest, func, args, _, _) ->
        let argStr = args |> List.map prettyPrintMIROperand |> String.concat ", "
        $"{prettyPrintMIRVReg dest} <- IndirectCall({prettyPrintMIROperand func}, [{argStr}])"
    | MIR.IndirectTailCall (func, args, _, _) ->
        let argStr = args |> List.map prettyPrintMIROperand |> String.concat ", "
        $"IndirectTailCall({prettyPrintMIROperand func}, [{argStr}])"
    | MIR.ClosureAlloc (dest, funcName, captures) ->
        let capsStr = captures |> List.map prettyPrintMIROperand |> String.concat ", "
        $"{prettyPrintMIRVReg dest} <- ClosureAlloc({funcName}, [{capsStr}])"
    | MIR.ClosureCall (dest, closure, args, _, _) ->
        let argStr = args |> List.map prettyPrintMIROperand |> String.concat ", "
        $"{prettyPrintMIRVReg dest} <- ClosureCall({prettyPrintMIROperand closure}, [{argStr}])"
    | MIR.ClosureTailCall (closure, args, _) ->
        let argStr = args |> List.map prettyPrintMIROperand |> String.concat ", "
        $"ClosureTailCall({prettyPrintMIROperand closure}, [{argStr}])"
    | MIR.HeapAlloc (dest, sizeBytes) ->
        $"{prettyPrintMIRVReg dest} <- HeapAlloc({sizeBytes})"
    | MIR.HeapStore (addr, offset, src, valueType) ->
        let baseText = $"HeapStore({prettyPrintMIRVReg addr}, {offset}, {prettyPrintMIROperand src})"
        appendTypeSuffix valueType baseText
    | MIR.HeapLoad (dest, addr, offset, valueType) ->
        let baseText = $"{prettyPrintMIRVReg dest} <- HeapLoad({prettyPrintMIRVReg addr}, {offset})"
        appendTypeSuffix valueType baseText
    | MIR.StringConcat (dest, left, right) ->
        $"{prettyPrintMIRVReg dest} <- StringConcat({prettyPrintMIROperand left}, {prettyPrintMIROperand right})"
    | MIR.RefCountInc (addr, payloadSize, kind) ->
        $"RefCountInc({prettyPrintMIRVReg addr}, size={payloadSize}, kind={prettyPrintMIRRcKind kind})"
    | MIR.RefCountDec (addr, payloadSize, kind) ->
        $"RefCountDec({prettyPrintMIRVReg addr}, size={payloadSize}, kind={prettyPrintMIRRcKind kind})"
    | MIR.Print (src, valueType) ->
        $"Print({prettyPrintMIROperand src}, type={valueType})"
    | MIR.RuntimeError message ->
        $"RuntimeError(\"{message}\")"
    | MIR.FileReadText (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileReadText({prettyPrintMIROperand path})"
    | MIR.FileExists (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileExists({prettyPrintMIROperand path})"
    | MIR.FileWriteText (dest, path, content) ->
        $"{prettyPrintMIRVReg dest} <- FileWriteText({prettyPrintMIROperand path}, {prettyPrintMIROperand content})"
    | MIR.FileAppendText (dest, path, content) ->
        $"{prettyPrintMIRVReg dest} <- FileAppendText({prettyPrintMIROperand path}, {prettyPrintMIROperand content})"
    | MIR.FileDelete (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileDelete({prettyPrintMIROperand path})"
    | MIR.FileSetExecutable (dest, path) ->
        $"{prettyPrintMIRVReg dest} <- FileSetExecutable({prettyPrintMIROperand path})"
    | MIR.FileWriteFromPtr (dest, path, ptr, length) ->
        $"{prettyPrintMIRVReg dest} <- FileWriteFromPtr({prettyPrintMIROperand path}, {prettyPrintMIROperand ptr}, {prettyPrintMIROperand length})"
    | MIR.FloatSqrt (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatSqrt({prettyPrintMIROperand src})"
    | MIR.FloatAbs (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatAbs({prettyPrintMIROperand src})"
    | MIR.FloatNeg (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatNeg({prettyPrintMIROperand src})"
    | MIR.Int64ToFloat (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- Int64ToFloat({prettyPrintMIROperand src})"
    | MIR.FloatToInt64 (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatToInt64({prettyPrintMIROperand src})"
    | MIR.FloatToBits (dest, src) ->
        $"{prettyPrintMIRVReg dest} <- FloatToBits({prettyPrintMIROperand src})"
    | MIR.RawAlloc (dest, numBytes) ->
        $"{prettyPrintMIRVReg dest} <- RawAlloc({prettyPrintMIROperand numBytes})"
    | MIR.RawFree ptr ->
        $"RawFree({prettyPrintMIROperand ptr})"
    | MIR.RawGet (dest, ptr, byteOffset, valueType) ->
        let baseText = $"{prettyPrintMIRVReg dest} <- RawGet({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset})"
        appendTypeSuffix valueType baseText
    | MIR.RawGetByte (dest, ptr, byteOffset) ->
        $"{prettyPrintMIRVReg dest} <- RawGetByte({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset})"
    | MIR.RawSet (ptr, byteOffset, value, valueType) ->
        let baseText = $"RawSet({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset}, {prettyPrintMIROperand value})"
        appendTypeSuffix valueType baseText
    | MIR.RawSetByte (ptr, byteOffset, value) ->
        $"RawSetByte({prettyPrintMIROperand ptr}, {prettyPrintMIROperand byteOffset}, {prettyPrintMIROperand value})"
    | MIR.RefCountIncString str ->
        $"RefCountIncString({prettyPrintMIROperand str})"
    | MIR.RefCountDecString str ->
        $"RefCountDecString({prettyPrintMIROperand str})"
    | MIR.RandomInt64 dest ->
        $"{prettyPrintMIRVReg dest} <- RandomInt64()"
    | MIR.DateNow dest ->
        $"{prettyPrintMIRVReg dest} <- DateNow()"
    | MIR.FloatToString (dest, value) ->
        $"{prettyPrintMIRVReg dest} <- FloatToString({prettyPrintMIROperand value})"
    | MIR.Phi (dest, sources, valueType) ->
        let srcStrs =
            sources
            |> List.map (fun (operand, label) -> $"({prettyPrintMIROperand operand}, {prettyPrintMIRLabel label})")
            |> String.concat ", "
        let baseText = $"{prettyPrintMIRVReg dest} <- Phi([{srcStrs}])"
        appendTypeSuffix valueType baseText
    | MIR.CoverageHit exprId ->
        $"CoverageHit({exprId})"

/// Pretty-print MIR terminator
let private prettyPrintMIRTerminator (term: MIR.Terminator) : string =
    match term with
    | MIR.Ret operand -> $"ret {prettyPrintMIROperand operand}"
    | MIR.Branch (cond, trueLabel, falseLabel) ->
        $"branch {prettyPrintMIROperand cond} ? {prettyPrintMIRLabel trueLabel} : {prettyPrintMIRLabel falseLabel}"
    | MIR.Jump label -> $"jump {prettyPrintMIRLabel label}"

/// Format MIR program with CFG structure
let formatMIR (program: MIR.Program) : string =
    let (MIR.Program (functions, _, _)) = program
    let prettyPrintBlock (block: MIR.BasicBlock) =
        let labelLine = $"  {prettyPrintMIRLabel block.Label}:"
        let instrLines = block.Instrs |> List.map prettyPrintMIRInstr |> List.map (fun line -> $"    {line}")
        let termLine = $"    {prettyPrintMIRTerminator block.Terminator}"
        String.concat "\n" (labelLine :: instrLines @ [termLine])

    let prettyPrintFunction (func: MIR.Function) =
        let entryLabel = func.CFG.Entry
        let entryBlock =
            Map.tryFind entryLabel func.CFG.Blocks
            |> Option.map (fun block -> (entryLabel, block))
        let otherBlocks =
            func.CFG.Blocks
            |> Map.remove entryLabel
            |> Map.toList
            |> List.sortBy (fun (label, _) -> prettyPrintMIRLabel label)
        let orderedBlocks =
            match entryBlock with
            | Some block -> block :: otherBlocks
            | None -> otherBlocks
        let blockLines =
            orderedBlocks
            |> List.map (fun (_, block) -> prettyPrintBlock block)
            |> String.concat "\n"
        if blockLines = "" then
            $"Function {func.Name}:\n  <empty>"
        else
            $"Function {func.Name}:\n{blockLines}"

    functions
    |> List.map prettyPrintFunction
    |> String.concat "\n\n"

/// Pretty-print LIR physical register
let private prettyPrintLIRPhysReg = function
    | LIR.X0 -> "X0" | LIR.X1 -> "X1" | LIR.X2 -> "X2" | LIR.X3 -> "X3"
    | LIR.X4 -> "X4" | LIR.X5 -> "X5" | LIR.X6 -> "X6" | LIR.X7 -> "X7"
    | LIR.X8 -> "X8" | LIR.X9 -> "X9" | LIR.X10 -> "X10" | LIR.X11 -> "X11"
    | LIR.X12 -> "X12" | LIR.X13 -> "X13" | LIR.X14 -> "X14" | LIR.X15 -> "X15"
    | LIR.X16 -> "X16" | LIR.X17 -> "X17"
    | LIR.X19 -> "X19" | LIR.X20 -> "X20" | LIR.X21 -> "X21" | LIR.X22 -> "X22"
    | LIR.X23 -> "X23" | LIR.X24 -> "X24" | LIR.X25 -> "X25" | LIR.X26 -> "X26"
    | LIR.X27 -> "X27"
    | LIR.X29 -> "X29" | LIR.X30 -> "X30" | LIR.SP -> "SP"

/// Pretty-print LIR register
let private prettyPrintLIRReg = function
    | LIR.Physical pr -> prettyPrintLIRPhysReg pr
    | LIR.Virtual n -> $"v{n}"

/// Pretty-print LIR FP physical register
let private prettyPrintLIRPhysFPReg = function
    | LIR.D0 -> "D0" | LIR.D1 -> "D1" | LIR.D2 -> "D2" | LIR.D3 -> "D3"
    | LIR.D4 -> "D4" | LIR.D5 -> "D5" | LIR.D6 -> "D6" | LIR.D7 -> "D7"
    | LIR.D8 -> "D8" | LIR.D9 -> "D9" | LIR.D10 -> "D10" | LIR.D11 -> "D11"
    | LIR.D12 -> "D12" | LIR.D13 -> "D13" | LIR.D14 -> "D14" | LIR.D15 -> "D15"

/// Pretty-print LIR FP register
let private prettyPrintLIRFReg = function
    | LIR.FPhysical pr -> prettyPrintLIRPhysFPReg pr
    | LIR.FVirtual n -> $"fv{n}"

/// Pretty-print LIR operand
let private prettyPrintLIROperand = function
    | LIR.Imm n -> $"Imm {n}"
    | LIR.FloatImm f -> $"FloatImm {f}"
    | LIR.Reg reg -> $"Reg {prettyPrintLIRReg reg}"
    | LIR.StackSlot n -> $"Stack {n}"
    | LIR.StringSymbol value -> $"str[{value}]"
    | LIR.FloatSymbol value -> $"float[{value}]"
    | LIR.FuncAddr name -> $"&{name}"

let private prettyPrintLIRRcKind = function
    | LIR.GenericHeap -> "generic"
    | LIR.TaggedList -> "list"

/// Pretty-print LIR instruction
let private prettyPrintLIRInstr (instr: LIR.Instr) : string =
    match instr with
    | LIR.Mov (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mov({prettyPrintLIROperand src})"
    | LIR.Phi (dest, sources, _) ->
        let srcs = sources |> List.map (fun (op, LIR.Label lbl) -> $"({prettyPrintLIROperand op}, {lbl})") |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- Phi([{srcs}])"
    | LIR.FPhi (dest, sources) ->
        let srcs = sources |> List.map (fun (freg, LIR.Label lbl) -> $"({prettyPrintLIRFReg freg}, {lbl})") |> String.concat ", "
        $"{prettyPrintLIRFReg dest} <- FPhi([{srcs}])"
    | LIR.Store (offset, src) ->
        $"Store(Stack {offset}, {prettyPrintLIRReg src})"
    | LIR.Add (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Add({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Sub (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sub({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Mul (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Mul({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Sdiv (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sdiv({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Udiv (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Udiv({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Msub (dest, mulLeft, mulRight, sub) ->
        $"{prettyPrintLIRReg dest} <- Msub({prettyPrintLIRReg mulLeft}, {prettyPrintLIRReg mulRight}, {prettyPrintLIRReg sub})"
    | LIR.Madd (dest, mulLeft, mulRight, add) ->
        $"{prettyPrintLIRReg dest} <- Madd({prettyPrintLIRReg mulLeft}, {prettyPrintLIRReg mulRight}, {prettyPrintLIRReg add})"
    | LIR.Cmp (left, right) ->
        $"Cmp({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Cset (dest, cond) ->
        $"{prettyPrintLIRReg dest} <- Cset({cond})"
    | LIR.And (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- And({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.And_imm (dest, src, imm) ->
        $"{prettyPrintLIRReg dest} <- And_imm({prettyPrintLIRReg src}, #{imm})"
    | LIR.Orr (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Orr({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Eor (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Eor({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Lsl (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsl({prettyPrintLIRReg src}, {prettyPrintLIRReg shift})"
    | LIR.Lsr (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsr({prettyPrintLIRReg src}, {prettyPrintLIRReg shift})"
    | LIR.Lsl_imm (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsl_imm({prettyPrintLIRReg src}, #{shift})"
    | LIR.Lsr_imm (dest, src, shift) ->
        $"{prettyPrintLIRReg dest} <- Lsr_imm({prettyPrintLIRReg src}, #{shift})"
    | LIR.Mvn (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mvn({prettyPrintLIRReg src})"
    | LIR.Sxtb (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Sxtb({prettyPrintLIRReg src})"
    | LIR.Sxth (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Sxth({prettyPrintLIRReg src})"
    | LIR.Sxtw (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Sxtw({prettyPrintLIRReg src})"
    | LIR.Uxtb (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Uxtb({prettyPrintLIRReg src})"
    | LIR.Uxth (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Uxth({prettyPrintLIRReg src})"
    | LIR.Uxtw (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Uxtw({prettyPrintLIRReg src})"
    | LIR.Call (dest, funcName, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- Call({funcName}, [{argStr}])"
    | LIR.TailCall (funcName, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"TailCall({funcName}, [{argStr}])"
    | LIR.IndirectCall (dest, func, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- IndirectCall({prettyPrintLIRReg func}, [{argStr}])"
    | LIR.IndirectTailCall (func, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"IndirectTailCall({prettyPrintLIRReg func}, [{argStr}])"
    | LIR.ClosureAlloc (dest, funcName, captures) ->
        let capsStr = captures |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- ClosureAlloc({funcName}, [{capsStr}])"
    | LIR.ClosureCall (dest, closure, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"{prettyPrintLIRReg dest} <- ClosureCall({prettyPrintLIRReg closure}, [{argStr}])"
    | LIR.ClosureTailCall (closure, args) ->
        let argStr = args |> List.map prettyPrintLIROperand |> String.concat ", "
        $"ClosureTailCall({prettyPrintLIRReg closure}, [{argStr}])"
    | LIR.SaveRegs (intRegs, floatRegs) ->
        let intStr = intRegs |> List.map (sprintf "%A") |> String.concat ", "
        let floatStr = floatRegs |> List.map (sprintf "%A") |> String.concat ", "
        $"SaveRegs([{intStr}], [{floatStr}])"
    | LIR.RestoreRegs (intRegs, floatRegs) ->
        let intStr = intRegs |> List.map (sprintf "%A") |> String.concat ", "
        let floatStr = floatRegs |> List.map (sprintf "%A") |> String.concat ", "
        $"RestoreRegs([{intStr}], [{floatStr}])"
    | LIR.ArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> sprintf "%A <- %s" dest (prettyPrintLIROperand src))
        sprintf "ArgMoves(%s)" (String.concat ", " moveStrs)
    | LIR.TailArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> sprintf "%A <- %s" dest (prettyPrintLIROperand src))
        sprintf "TailArgMoves(%s)" (String.concat ", " moveStrs)
    | LIR.FArgMoves moves ->
        let moveStrs = moves |> List.map (fun (dest, src) -> sprintf "%A <- %s" dest (prettyPrintLIRFReg src))
        sprintf "FArgMoves(%s)" (String.concat ", " moveStrs)
    | LIR.PrintInt64 reg ->
        $"PrintInt64({prettyPrintLIRReg reg})"
    | LIR.PrintBool reg ->
        $"PrintBool({prettyPrintLIRReg reg})"
    | LIR.PrintFloat freg ->
        $"PrintFloat({prettyPrintLIRFReg freg})"
    | LIR.PrintString value ->
        $"PrintString(str[{value}], len={value.Length})"
    | LIR.RuntimeError message ->
        $"RuntimeError(\"{message}\")"
    | LIR.PrintChars chars ->
        let s = chars |> List.map (fun b -> char b) |> System.String.Concat
        $"PrintChars(\"{s}\")"
    | LIR.PrintBytes reg ->
        $"PrintBytes({prettyPrintLIRReg reg})"
    | LIR.PrintInt64NoNewline reg ->
        $"PrintIntNoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintBoolNoNewline reg ->
        $"PrintBoolNoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintFloatNoNewline freg ->
        $"PrintFloatNoNewline({prettyPrintLIRFReg freg})"
    | LIR.PrintHeapStringNoNewline reg ->
        $"PrintHeapStringNoNewline({prettyPrintLIRReg reg})"
    | LIR.PrintList (listPtr, elemType) ->
        $"PrintList({prettyPrintLIRReg listPtr}, {elemType})"
    | LIR.PrintSum (sumPtr, variants) ->
        $"PrintSum({prettyPrintLIRReg sumPtr}, {variants})"
    | LIR.PrintRecord (recordPtr, typeName, fields) ->
        $"PrintRecord({prettyPrintLIRReg recordPtr}, {typeName}, {fields})"
    | LIR.Exit -> "Exit"
    | LIR.FMov (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FMov({prettyPrintLIRFReg src})"
    | LIR.FLoad (dest, value) ->
        $"{prettyPrintLIRFReg dest} <- FLoad(float[{value}])"
    | LIR.FAdd (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FAdd({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FSub (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FSub({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FMul (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FMul({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FDiv (dest, left, right) ->
        $"{prettyPrintLIRFReg dest} <- FDiv({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.FNeg (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FNeg({prettyPrintLIRFReg src})"
    | LIR.FAbs (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FAbs({prettyPrintLIRFReg src})"
    | LIR.FSqrt (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- FSqrt({prettyPrintLIRFReg src})"
    | LIR.FCmp (left, right) ->
        $"FCmp({prettyPrintLIRFReg left}, {prettyPrintLIRFReg right})"
    | LIR.Int64ToFloat (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- Int64ToFloat({prettyPrintLIRReg src})"
    | LIR.FloatToInt64 (dest, src) ->
        $"{prettyPrintLIRReg dest} <- FloatToInt64({prettyPrintLIRFReg src})"
    | LIR.FloatToBits (dest, src) ->
        $"{prettyPrintLIRReg dest} <- FloatToBits({prettyPrintLIRFReg src})"
    | LIR.GpToFp (dest, src) ->
        $"{prettyPrintLIRFReg dest} <- GpToFp({prettyPrintLIRReg src})"
    | LIR.FpToGp (dest, src) ->
        $"{prettyPrintLIRReg dest} <- FpToGp({prettyPrintLIRFReg src})"
    | LIR.HeapAlloc (dest, sizeBytes) ->
        $"{prettyPrintLIRReg dest} <- HeapAlloc({sizeBytes})"
    | LIR.HeapStore (addr, offset, src, _valueType) ->
        $"HeapStore({prettyPrintLIRReg addr}, {offset}, {prettyPrintLIROperand src})"
    | LIR.HeapLoad (dest, addr, offset) ->
        $"{prettyPrintLIRReg dest} <- HeapLoad({prettyPrintLIRReg addr}, {offset})"
    | LIR.RefCountInc (addr, payloadSize, kind) ->
        $"RefCountInc({prettyPrintLIRReg addr}, {payloadSize}, {prettyPrintLIRRcKind kind})"
    | LIR.RefCountDec (addr, payloadSize, kind) ->
        $"RefCountDec({prettyPrintLIRReg addr}, {payloadSize}, {prettyPrintLIRRcKind kind})"
    | LIR.StringConcat (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- StringConcat({prettyPrintLIROperand left}, {prettyPrintLIROperand right})"
    | LIR.PrintHeapString reg ->
        $"PrintHeapString({prettyPrintLIRReg reg})"
    | LIR.LoadFuncAddr (dest, funcName) ->
        $"{prettyPrintLIRReg dest} <- LoadFuncAddr({funcName})"
    | LIR.FileReadText (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileReadText({prettyPrintLIROperand path})"
    | LIR.FileExists (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileExists({prettyPrintLIROperand path})"
    | LIR.FileWriteText (dest, path, content) ->
        $"{prettyPrintLIRReg dest} <- FileWriteText({prettyPrintLIROperand path}, {prettyPrintLIROperand content})"
    | LIR.FileAppendText (dest, path, content) ->
        $"{prettyPrintLIRReg dest} <- FileAppendText({prettyPrintLIROperand path}, {prettyPrintLIROperand content})"
    | LIR.FileDelete (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileDelete({prettyPrintLIROperand path})"
    | LIR.FileSetExecutable (dest, path) ->
        $"{prettyPrintLIRReg dest} <- FileSetExecutable({prettyPrintLIROperand path})"
    | LIR.FileWriteFromPtr (dest, path, ptr, length) ->
        $"{prettyPrintLIRReg dest} <- FileWriteFromPtr({prettyPrintLIROperand path}, {prettyPrintLIRReg ptr}, {prettyPrintLIRReg length})"
    | LIR.RawAlloc (dest, numBytes) ->
        $"{prettyPrintLIRReg dest} <- RawAlloc({prettyPrintLIRReg numBytes})"
    | LIR.RawFree ptr ->
        $"RawFree({prettyPrintLIRReg ptr})"
    | LIR.RawGet (dest, ptr, byteOffset) ->
        $"{prettyPrintLIRReg dest} <- RawGet({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset})"
    | LIR.RawGetByte (dest, ptr, byteOffset) ->
        $"{prettyPrintLIRReg dest} <- RawGetByte({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset})"
    | LIR.RawSet (ptr, byteOffset, value, valueType) ->
        let baseText = $"RawSet({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset}, {prettyPrintLIRReg value})"
        match valueType with
        | Some typ -> $"{baseText} : {typ}"
        | None -> baseText
    | LIR.RawSetByte (ptr, byteOffset, value) ->
        $"RawSetByte({prettyPrintLIRReg ptr}, {prettyPrintLIRReg byteOffset}, {prettyPrintLIRReg value})"
    | LIR.RefCountIncString str ->
        $"RefCountIncString({prettyPrintLIROperand str})"
    | LIR.RefCountDecString str ->
        $"RefCountDecString({prettyPrintLIROperand str})"
    | LIR.RandomInt64 dest ->
        $"{prettyPrintLIRReg dest} <- RandomInt64()"
    | LIR.DateNow dest ->
        $"{prettyPrintLIRReg dest} <- DateNow()"
    | LIR.FloatToString (dest, value) ->
        $"{prettyPrintLIRReg dest} <- FloatToString({prettyPrintLIRFReg value})"
    | LIR.CoverageHit exprId ->
        $"CoverageHit({exprId})"

/// Pretty-print symbolic LIR terminator
let private prettyPrintLIRTerminator (term: LIR.Terminator) : string =
    match term with
    | LIR.Ret -> "Ret"
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        $"Branch({prettyPrintLIRReg cond}, {trueLabel}, {falseLabel})"
    | LIR.BranchZero (cond, zeroLabel, nonZeroLabel) ->
        $"BranchZero({prettyPrintLIRReg cond}, {zeroLabel}, {nonZeroLabel})"
    | LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) ->
        $"BranchBitZero({prettyPrintLIRReg reg}, #{bit}, {zeroLabel}, {nonZeroLabel})"
    | LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) ->
        $"BranchBitNonZero({prettyPrintLIRReg reg}, #{bit}, {nonZeroLabel}, {zeroLabel})"
    | LIR.CondBranch (cond, trueLabel, falseLabel) ->
        $"CondBranch({cond}, {trueLabel}, {falseLabel})"
    | LIR.Jump label -> $"Jump({label})"

/// Format symbolic LIR program with CFG structure
let formatLIR (LIR.Program functions) : string =
    let prettyPrintCalleeSaved (regs: LIR.PhysReg list) : string =
        regs
        |> List.map prettyPrintLIRPhysReg
        |> String.concat ", "

    let funcStrs =
        functions
        |> List.map (fun func ->
            let blockStrs =
                func.CFG.Blocks
                |> Map.toList
                |> List.sortBy fst
                |> List.map (fun (label, block) ->
                    let instrStrs =
                        block.Instrs
                        |> List.map prettyPrintLIRInstr
                        |> List.map (sprintf "    %s")
                        |> String.concat "\n"
                    let termStr = sprintf "    %s" (prettyPrintLIRTerminator block.Terminator)
                    $"  {label}:\n{instrStrs}\n{termStr}")
                |> String.concat "\n"
            let calleeSavedText = prettyPrintCalleeSaved func.UsedCalleeSaved
            $"{func.Name}:\n  StackSize: {func.StackSize}\n  UsedCalleeSaved: [{calleeSavedText}]\n{blockStrs}")
        |> String.concat "\n\n"
    funcStrs
