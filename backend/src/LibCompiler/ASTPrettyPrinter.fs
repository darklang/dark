// ASTPrettyPrinter.fs - Pretty printers for Darklang source syntaxes.
//
// Formats the shared AST into compiler syntax or interpreter syntax.

module ASTPrettyPrinter

open AST

type Syntax =
    | CompilerSyntax
    | InterpreterSyntax

let private escapeStringContent (input: string) : string =
    input
    |> String.collect (fun c ->
        match c with
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | '\000' -> "\\0"
        | _ -> string c)

let private escapeCharContent (input: string) : string =
    input
    |> String.collect (fun c ->
        match c with
        | '\\' -> "\\\\"
        | '\'' -> "\\'"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | '\000' -> "\\0"
        | _ -> string c)

let private formatFloatLiteral (value: float) : string =
    let raw = value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    let containsLetters = raw |> Seq.exists System.Char.IsLetter
    if containsLetters || raw.Contains(".") || raw.Contains("E") || raw.Contains("e") then
        raw
    else
        $"{raw}.0"

let private reservedIdentifierNames : Set<string> =
    set [
        "let"
        "in"
        "if"
        "then"
        "else"
        "def"
        "type"
        "of"
        "match"
        "with"
        "when"
        "fun"
        "true"
        "false"
        "_"
    ]

let private isIdentifierStartChar (c: char) : bool =
    System.Char.IsLetter(c) || c = '_'

let private isIdentifierContinueChar (c: char) : bool =
    System.Char.IsLetterOrDigit(c) || c = '_'

let private isBareIdentifierSegment (name: string) : bool =
    name.Length > 0
    && isIdentifierStartChar name[0]
    && (name |> Seq.forall isIdentifierContinueChar)
    && not (Set.contains name reservedIdentifierNames)

let private formatIdentifierSegment (name: string) : string =
    if isBareIdentifierSegment name then
        name
    else
        $"``{name}``"

let private formatIdentifierPath (name: string) : string =
    if name.Contains "." then
        name.Split('.')
        |> Array.toList
        |> List.map formatIdentifierSegment
        |> String.concat "."
    else
        formatIdentifierSegment name

let rec private formatType (typ: Type) : string =
    match typ with
    | TInt8 -> "Int8"
    | TInt16 -> "Int16"
    | TInt32 -> "Int32"
    | TInt64 -> "Int64"
    | TInt128 -> "Int128"
    | TUInt8 -> "UInt8"
    | TUInt16 -> "UInt16"
    | TUInt32 -> "UInt32"
    | TUInt64 -> "UInt64"
    | TUInt128 -> "UInt128"
    | TBool -> "Bool"
    | TFloat64 -> "Float"
    | TString -> "String"
    | TBytes -> "Bytes"
    | TChar -> "Char"
    | TUnit -> "Unit"
    | TRuntimeError -> "RuntimeError"
    | TRawPtr -> "RawPtr"
    | TVar name -> formatIdentifierSegment name
    | TList elemType -> $"List<{formatType elemType}>"
    | TDict (keyType, valueType) -> $"Dict<{formatType keyType}, {formatType valueType}>"
    | TTuple elemTypes ->
        let elemText = elemTypes |> List.map formatType |> String.concat ", "
        $"({elemText})"
    | TRecord (name, []) -> formatIdentifierPath name
    | TRecord (name, typeArgs) ->
        let argsText = typeArgs |> List.map formatType |> String.concat ", "
        $"{formatIdentifierPath name}<{argsText}>"
    | TSum (name, []) -> formatIdentifierPath name
    | TSum (name, typeArgs) ->
        let argsText = typeArgs |> List.map formatType |> String.concat ", "
        $"{formatIdentifierPath name}<{argsText}>"
    | TFunction (paramTypes, returnType) ->
        let paramsText = paramTypes |> List.map formatType |> String.concat ", "
        $"({paramsText}) -> {formatType returnType}"

let private formatBinOp (op: BinOp) : string =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Shl -> "<<"
    | Shr -> ">>"
    | BitAnd -> "&"
    | BitOr -> "|||"
    | BitXor -> "^"
    | StringConcat -> "++"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Lte -> "<="
    | Gte -> ">="
    | And -> "&&"
    | Or -> "||"

let private formatUnaryOp (op: UnaryOp) : string =
    match op with
    | Neg -> "-"
    | Not -> "!"
    | BitNot -> "~~~"

let private isComparisonOp (op: BinOp) : bool =
    match op with
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte -> true
    | _ -> false

let private binOpPrecedence (op: BinOp) : int =
    match op with
    | Or -> 1
    | And -> 2
    | BitOr -> 3
    | BitXor -> 4
    | BitAnd -> 5
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte -> 6
    | Shl
    | Shr -> 7
    | Add
    | Sub
    | StringConcat -> 8
    | Mul
    | Div
    | Mod -> 9

let private shouldParenthesizeBinChild (parentOp: BinOp) (isLeftChild: bool) (childOp: BinOp) : bool =
    let parentPrec = binOpPrecedence parentOp
    let childPrec = binOpPrecedence childOp
    if childPrec < parentPrec then
        true
    elif childPrec > parentPrec then
        false
    elif isComparisonOp parentOp then
        true
    else
        // Operators are left-associative: left child can omit equal-precedence
        // parentheses, right child needs them to preserve tree shape.
        not isLeftChild

let rec private isAtomicExpr (expr: Expr) : bool =
    match expr with
    | UnitLiteral
    | Int64Literal _
    | Int128Literal _
    | Int8Literal _
    | Int16Literal _
    | Int32Literal _
    | UInt8Literal _
    | UInt16Literal _
    | UInt32Literal _
    | UInt64Literal _
    | UInt128Literal _
    | BoolLiteral _
    | StringLiteral _
    | CharLiteral _
    | FloatLiteral _
    | InterpolatedString _
    | Var _
    | FuncRef _
    | Call _
    | TypeApp _
    | Apply _
    | TupleLiteral _
    | RecordLiteral _
    | ListLiteral _
    | Constructor (_, _, None) -> true
    | TupleAccess (tupleExpr, _) -> isAtomicExpr tupleExpr
    | RecordAccess (recordExpr, _) -> isAtomicExpr recordExpr
    | _ -> false

let private parenthesizeIfNeeded (expr: Expr) (text: string) : string =
    if isAtomicExpr expr then text else $"({text})"

let private parenthesizeTupleBaseIfNeeded (expr: Expr) (text: string) : string =
    match expr with
    | TupleAccess _ -> $"({text})"
    | _ -> parenthesizeIfNeeded expr text

let private isSyntheticUnitParam ((paramName, paramType): string * Type) : bool =
    paramType = TUnit && paramName.StartsWith("$unit")

let private isSyntheticUnitParamList (parameters: NonEmptyList<string * Type>) : bool =
    match NonEmptyList.toList parameters with
    | [singleParam] -> isSyntheticUnitParam singleParam
    | _ -> false

let private isUnitArgumentList (args: NonEmptyList<Expr>) : bool =
    match NonEmptyList.toList args with
    | [UnitLiteral] -> true
    | _ -> false

let private tryParseInterpreterLambdaTypeVar (typeVarName: string) : (int * int) option =
    let prefix = "__interp_lambda_"
    if not (typeVarName.StartsWith prefix) then
        None
    else
        let remainder = typeVarName.Substring(prefix.Length)
        let firstSeparator = remainder.IndexOf '_'
        if firstSeparator < 0 then
            None
        else
            let secondSeparator = remainder.IndexOf('_', firstSeparator + 1)
            if secondSeparator < 0 then
                None
            else
                let seedText = remainder.Substring(0, firstSeparator)
                let indexText =
                    remainder.Substring(firstSeparator + 1, secondSeparator - firstSeparator - 1)

                match System.Int32.TryParse seedText, System.Int32.TryParse indexText with
                | (true, seed), (true, paramIndex) -> Some (seed, paramIndex)
                | _ -> None

let private trySingleInterpreterLambdaParamInfo
    (parameters: NonEmptyList<string * Type>)
    : ((string * Type) * (int * int)) option =
    match NonEmptyList.toList parameters with
    | [singleParam] ->
        let (_, paramType) = singleParam
        match paramType with
        | TVar typeVarName ->
            tryParseInterpreterLambdaTypeVar typeVarName
            |> Option.map (fun info -> (singleParam, info))
        | _ -> None
    | _ -> None

let private tryCollectImplicitCurriedLambdaParameters
    (parameters: NonEmptyList<string * Type>)
    (body: Expr)
    : ((string * Type) list * Expr) option =
    match trySingleInterpreterLambdaParamInfo parameters with
    | None -> None
    | Some (firstParam, (seed, startIndex)) ->
        let rec collect
            (expectedIndex: int)
            (collectedRev: (string * Type) list)
            (currentExpr: Expr)
            : ((string * Type) list * Expr) =
            match currentExpr with
            | Lambda (nextParameters, nextBody) ->
                match trySingleInterpreterLambdaParamInfo nextParameters with
                | Some (nextParam, (nextSeed, nextIndex))
                    when nextSeed = seed && nextIndex = expectedIndex ->
                    collect (expectedIndex + 1) (nextParam :: collectedRev) nextBody
                | _ -> (List.rev collectedRev, currentExpr)
            | _ -> (List.rev collectedRev, currentExpr)

        let (collected, finalBody) = collect (startIndex + 1) [firstParam] body
        if List.length collected > 1 then Some (collected, finalBody) else None

let rec private formatPattern (syntax: Syntax) (pattern: Pattern) : string =
    match pattern with
    | PUnit -> "()"
    | PWildcard -> "_"
    | PVar name -> formatIdentifierSegment name
    | PConstructor (name, None) -> formatIdentifierPath name
    | PConstructor (name, Some payload) ->
        let payloadText = formatPattern syntax payload
        match syntax with
        | CompilerSyntax -> $"{formatIdentifierPath name}({payloadText})"
        | InterpreterSyntax ->
            if payloadText.StartsWith "(" then
                $"{formatIdentifierPath name} {payloadText}"
            else
                $"{formatIdentifierPath name} {payloadText}"
    | PInt64 n ->
        match syntax with
        | CompilerSyntax -> $"{n}"
        | InterpreterSyntax -> $"{n}L"
    | PInt128Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}Q"
        | InterpreterSyntax -> $"{n}Q"
    | PInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}y"
        | InterpreterSyntax -> $"{n}y"
    | PInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}s"
        | InterpreterSyntax -> $"{n}s"
    | PInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}l"
        | InterpreterSyntax -> $"{n}l"
    | PUInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}uy"
        | InterpreterSyntax -> $"{n}uy"
    | PUInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}us"
        | InterpreterSyntax -> $"{n}us"
    | PUInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}ul"
        | InterpreterSyntax -> $"{n}ul"
    | PUInt64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}UL"
        | InterpreterSyntax -> $"{n}UL"
    | PUInt128Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}Z"
        | InterpreterSyntax -> $"{n}Z"
    | PBool b -> if b then "true" else "false"
    | PString s -> $"\"{escapeStringContent s}\""
    | PChar c -> $"'{escapeCharContent c}'"
    | PFloat f -> formatFloatLiteral f
    | PTuple patterns ->
        let parts = patterns |> List.map (formatPattern syntax) |> String.concat ", "
        $"({parts})"
    | PRecord (typeName, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (name, fieldPattern) ->
                $"{formatIdentifierSegment name} = {formatPattern syntax fieldPattern}")
            |> String.concat ", "
        if typeName = "" then
            $"{{ {fieldsText} }}"
        else
            $"{formatIdentifierPath typeName} {{ {fieldsText} }}"
    | PList patterns ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let items = patterns |> List.map (formatPattern syntax) |> String.concat separator
        $"[{items}]"
    | PListCons (head, tail) ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let headText = head |> List.map (formatPattern syntax) |> String.concat separator
        if headText = "" then
            $"[...{formatPattern syntax tail}]"
        else
            $"[{headText}{separator}...{formatPattern syntax tail}]"

let rec private formatExpr (syntax: Syntax) (expr: Expr) : string =
    let isNegativeNumericLiteral (arg: Expr) : bool =
        match arg with
        | Int64Literal n -> n < 0L
        | Int128Literal n -> n < System.Int128.Zero
        | Int8Literal n -> n < 0y
        | Int16Literal n -> n < 0s
        | Int32Literal n -> n < 0l
        | FloatLiteral f ->
            // Keep -0.0 wrapped as well; it is lexically ambiguous in application position.
            System.BitConverter.DoubleToInt64Bits(f) < 0L
        | _ -> false

    let formatAppArg (arg: Expr) : string =
        let argText = formatExpr syntax arg
        match syntax with
        | CompilerSyntax ->
            parenthesizeIfNeeded arg argText
        | InterpreterSyntax ->
            match arg with
            | _ when isNegativeNumericLiteral arg -> $"({argText})"
            | Constructor (_, _, None) -> $"({argText})"
            | TupleLiteral _ -> $"({argText})"
            | Call _
            | TypeApp _
            | Apply _ -> $"({argText})"
            | _ -> parenthesizeIfNeeded arg argText

    let rec formatInterpreterAppArgs (args: Expr list) : string list =
        match args with
        | [] -> []
        | [lastArg] -> [formatAppArg lastArg]
        | currentArg :: ((UnitLiteral as nextArg) :: restArgs) ->
            // `f x ()` is ambiguous with zero-arg calls (`x()`).
            // Parenthesize the preceding argument to preserve argument boundaries.
            $"({formatAppArg currentArg})"
            :: (formatInterpreterAppArgs (nextArg :: restArgs))
        | currentArg :: ((TupleLiteral _ as nextArg) :: restArgs) ->
            // `f g (a, b)` can be reparsed as applying `g` to tuple elements.
            // Parenthesize the preceding argument to keep tuple as a separate argument.
            $"({formatAppArg currentArg})"
            :: (formatInterpreterAppArgs (nextArg :: restArgs))
        | currentArg :: restArgs ->
            formatAppArg currentArg :: formatInterpreterAppArgs restArgs

    match expr with
    | UnitLiteral -> "()"
    | Int64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}"
        | InterpreterSyntax -> $"{n}L"
    | Int128Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}Q"
        | InterpreterSyntax -> $"{n}Q"
    | Int8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}y"
        | InterpreterSyntax -> $"{n}y"
    | Int16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}s"
        | InterpreterSyntax -> $"{n}s"
    | Int32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}l"
        | InterpreterSyntax -> $"{n}l"
    | UInt8Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}uy"
        | InterpreterSyntax -> $"{n}uy"
    | UInt16Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}us"
        | InterpreterSyntax -> $"{n}us"
    | UInt32Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}ul"
        | InterpreterSyntax -> $"{n}ul"
    | UInt64Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}UL"
        | InterpreterSyntax -> $"{n}UL"
    | UInt128Literal n ->
        match syntax with
        | CompilerSyntax -> $"{n}Z"
        | InterpreterSyntax -> $"{n}Z"
    | BoolLiteral b -> if b then "true" else "false"
    | StringLiteral s -> $"\"{escapeStringContent s}\""
    | CharLiteral c -> $"'{escapeCharContent c}'"
    | FloatLiteral f -> formatFloatLiteral f
    | InterpolatedString parts ->
        let partsText =
            parts
            |> List.map (function
                | StringText t -> escapeStringContent t
                | StringExpr e -> $"{{{formatExpr syntax e}}}")
            |> String.concat ""
        $"$\"{partsText}\""
    | BinOp (op, left, right) ->
        let formatChild (isLeftChild: bool) (child: Expr) : string =
            let childText = formatExpr syntax child
            match child with
            | BinOp (childOp, _, _) ->
                if shouldParenthesizeBinChild op isLeftChild childOp then
                    $"({childText})"
                else
                    childText
            | _ -> parenthesizeIfNeeded child childText
        let leftCanConsumeNegativeNumericArg (expr: Expr) : bool =
            match expr with
            | Var funcName when funcName.Contains "." -> true
            | Call _
            | TypeApp _
            | Apply _
            | Constructor (_, _, None) -> true
            | _ -> false
        let isNumericLiteralExpr (expr: Expr) : bool =
            match expr with
            | Int64Literal _
            | Int128Literal _
            | Int8Literal _
            | Int16Literal _
            | Int32Literal _
            | UInt8Literal _
            | UInt16Literal _
            | UInt32Literal _
            | UInt64Literal _
            | UInt128Literal _
            | FloatLiteral _ -> true
            | _ -> false
        let leftText = formatChild true left
        let rightTextBase = formatChild false right
        let rightText =
            match syntax, op with
            | InterpreterSyntax, Sub when leftCanConsumeNegativeNumericArg left && isNumericLiteralExpr right ->
                $"({rightTextBase})"
            | _ -> rightTextBase
        $"{leftText} {formatBinOp op} {rightText}"
    | UnaryOp (op, inner) ->
        let innerText = parenthesizeIfNeeded inner (formatExpr syntax inner)
        $"{formatUnaryOp op}{innerText}"
    | Let (name, value, body) ->
        $"let {formatIdentifierSegment name} = {formatExpr syntax value} in {formatExpr syntax body}"
    | Var name -> formatIdentifierPath name
    | If (cond, thenBranch, elseBranch) ->
        $"if {formatExpr syntax cond} then {formatExpr syntax thenBranch} else {formatExpr syntax elseBranch}"
    | Call (funcName, args) ->
        let argsList = NonEmptyList.toList args
        let formattedName = formatIdentifierPath funcName
        match syntax with
        | CompilerSyntax ->
            if isUnitArgumentList args then
                $"{formattedName}()"
            else
                let argsText = argsList |> List.map (formatExpr syntax) |> String.concat ", "
                $"{formattedName}({argsText})"
        | InterpreterSyntax ->
            if isUnitArgumentList args then
                $"{formattedName}()"
            else
                let argsText = argsList |> formatInterpreterAppArgs |> String.concat " "
                $"{formattedName} {argsText}"
    | TypeApp (funcName, typeArgs, args) ->
        let argsList = NonEmptyList.toList args
        let typeArgsText = typeArgs |> List.map formatType |> String.concat ", "
        let formattedName = formatIdentifierPath funcName
        match syntax with
        | CompilerSyntax ->
            if isUnitArgumentList args then
                $"{formattedName}<{typeArgsText}>()"
            else
                let argsText = argsList |> List.map (formatExpr syntax) |> String.concat ", "
                $"{formattedName}<{typeArgsText}>({argsText})"
        | InterpreterSyntax ->
            let head = $"{formattedName}<{typeArgsText}>"
            if isUnitArgumentList args then
                $"{head}()"
            else
                let argsText = argsList |> List.map (formatExpr syntax) |> String.concat ", "
                $"{head}({argsText})"
    | TupleLiteral elements ->
        let elementsText = elements |> List.map (formatExpr syntax) |> String.concat ", "
        $"({elementsText})"
    | TupleAccess (tupleExpr, index) ->
        let tupleBaseText = formatExpr syntax tupleExpr
        let tupleText =
            match syntax, tupleExpr with
            | InterpreterSyntax, (Call _ | TypeApp _ | Apply _) ->
                // In interpreter syntax, call application has no mandatory wrapping.
                // Parenthesize before postfix access so `.0` binds to the call result.
                $"({tupleBaseText})"
            | _ ->
                parenthesizeTupleBaseIfNeeded tupleExpr tupleBaseText
        $"{tupleText}.{index}"
    | RecordLiteral (typeName, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (name, value) ->
                $"{formatIdentifierSegment name} = {formatExpr syntax value}")
            |> String.concat ", "
        if typeName = "" then
            $"{{ {fieldsText} }}"
        else
            $"{formatIdentifierPath typeName} {{ {fieldsText} }}"
    | RecordUpdate (recordExpr, updates) ->
        let recordText = formatExpr syntax recordExpr
        let updatesText =
            updates
            |> List.map (fun (name, value) ->
                $"{formatIdentifierSegment name} = {formatExpr syntax value}")
            |> String.concat ", "
        $"{{ {recordText} with {updatesText} }}"
    | RecordAccess (recordExpr, fieldName) ->
        let recordBaseText = formatExpr syntax recordExpr
        let recordText =
            match syntax, recordExpr with
            | InterpreterSyntax, (Call _ | TypeApp _ | Apply _) ->
                // Same ambiguity as tuple access: ensure `.field` applies to call result.
                $"({recordBaseText})"
            | _ ->
                parenthesizeIfNeeded recordExpr recordBaseText
        $"{recordText}.{formatIdentifierSegment fieldName}"
    | Constructor (typeName, variantName, payload) ->
        let fullName =
            let formattedVariantName = formatIdentifierSegment variantName
            if typeName = "" then
                formattedVariantName
            else
                $"{formatIdentifierPath typeName}.{formattedVariantName}"
        match payload with
        | None -> fullName
        | Some payloadExpr ->
            let payloadText = formatAppArg payloadExpr
            match syntax with
            | CompilerSyntax -> $"{fullName}({formatExpr syntax payloadExpr})"
            | InterpreterSyntax -> $"{fullName} {payloadText}"
    | Match (scrutinee, cases) ->
        let scrutineeText = formatExpr syntax scrutinee
        let formatCaseBody (body: Expr) : string =
            let bodyText = formatExpr syntax body
            match body with
            // Without parens, nested match case bars get parsed as outer cases.
            | Match _
            | Let _ -> $"({bodyText})"
            | _ -> bodyText
        let caseText =
            cases
            |> List.map (fun case ->
                let patternsText =
                    case.Patterns
                    |> NonEmptyList.toList
                    |> List.map (formatPattern syntax)
                    |> String.concat " | "
                let guardText =
                    match case.Guard with
                    | None -> ""
                    | Some guardExpr -> $" when {formatExpr syntax guardExpr}"
                $"| {patternsText}{guardText} -> {formatCaseBody case.Body}")
            |> String.concat " "
        $"match {scrutineeText} with {caseText}"
    | ListLiteral elements ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let elementsText = elements |> List.map (formatExpr syntax) |> String.concat separator
        $"[{elementsText}]"
    | ListCons (head, tail) ->
        let separator =
            match syntax with
            | CompilerSyntax -> ", "
            | InterpreterSyntax -> "; "
        let headText = head |> List.map (formatExpr syntax) |> String.concat separator
        if headText = "" then
            $"[...{formatExpr syntax tail}]"
        else
            $"[{headText}{separator}...{formatExpr syntax tail}]"
    | Lambda (parameters, body) ->
        let parameterList = NonEmptyList.toList parameters
        match syntax with
        | CompilerSyntax ->
            match parameterList, body with
            | [ (paramName, TBool) ], BinOp (And, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                $"(&&) {formatAppArg rightArg}"
            | [ (paramName, TBool) ], BinOp (Or, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                $"(||) {formatAppArg rightArg}"
            | _ ->
                if isSyntheticUnitParamList parameters then
                    $"() => {formatExpr syntax body}"
                else
                    let paramsText =
                        parameterList
                        |> List.map (fun (name, typ) ->
                            $"{formatIdentifierSegment name}: {formatType typ}")
                        |> String.concat ", "
                    $"({paramsText}) => {formatExpr syntax body}"
        | InterpreterSyntax ->
            if isSyntheticUnitParamList parameters then
                $"fun () -> {formatExpr syntax body}"
            else
                match parameterList, body with
                | [ (paramName, TBool) ], BinOp (And, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                    $"(&&) {formatAppArg rightArg}"
                | [ (paramName, TBool) ], BinOp (Or, Var varName, rightArg) when paramName = "$pipe_arg" && varName = "$pipe_arg" ->
                    $"(||) {formatAppArg rightArg}"
                | _ ->
                    let parametersToFormat, bodyToFormat =
                        match tryCollectImplicitCurriedLambdaParameters parameters body with
                        | Some flattened -> flattened
                        | None -> (parameterList, body)

                    let paramsText =
                        parametersToFormat
                        |> List.map (fun (name, typ) ->
                            match typ with
                            | TVar typeVar ->
                                match tryParseInterpreterLambdaTypeVar typeVar with
                                | Some _ -> formatIdentifierSegment name
                                | None ->
                                    $"({formatIdentifierSegment name}: {formatType typ})"
                            | _ ->
                                $"({formatIdentifierSegment name}: {formatType typ})")
                        |> String.concat " "
                    $"fun {paramsText} -> {formatExpr syntax bodyToFormat}"
    | Apply (funcExpr, args) ->
        let argsList = NonEmptyList.toList args
        match syntax with
        | CompilerSyntax ->
            let funcText =
                match funcExpr with
                // Preserve Apply-vs-Constructor distinction for compiler parser
                // (`Constructor(arg)` parses as constructor payload, not Apply).
                | Constructor (_, _, None) -> $"({formatExpr syntax funcExpr})"
                | _ -> parenthesizeIfNeeded funcExpr (formatExpr syntax funcExpr)
            if isUnitArgumentList args then
                $"{funcText}()"
            else
                let argsText = argsList |> List.map (formatExpr syntax) |> String.concat ", "
                $"{funcText}({argsText})"
        | InterpreterSyntax ->
            match funcExpr, argsList with
            // Preserve Apply-vs-Constructor distinction for interpreter parser
            // by printing constructor application in pipe form.
            | Constructor (_, _, None), [singleArg] ->
                $"{formatExpr syntax singleArg} |> {formatExpr syntax funcExpr}"
            | TupleLiteral _, _ ->
                let funcText = formatExpr syntax funcExpr
                if isUnitArgumentList args then
                    $"{funcText}()"
                elif List.length argsList > 1 then
                    // Tuple-callee apply is only parseable in interpreter syntax
                    // via parenthesized call-arg form: (tupleExpr)(a, b).
                    let argsText = argsList |> List.map (formatExpr syntax) |> String.concat ", "
                    $"{funcText}({argsText})"
                else
                    let argsText = argsList |> formatInterpreterAppArgs |> String.concat " "
                    $"{funcText} {argsText}"
            | Lambda _, _ when List.length argsList > 1 ->
                // Preserve uncurried multi-arg lambda-apply shape.
                let funcText = parenthesizeIfNeeded funcExpr (formatExpr syntax funcExpr)
                let argsText = argsList |> List.map (formatExpr syntax) |> String.concat ", "
                $"{funcText}({argsText})"
            | Apply _, _ when List.length argsList > 1 ->
                // Preserve grouped argument shape for nested apply chains that
                // originated from parenthesized multi-arg application.
                let funcText = formatExpr syntax funcExpr
                let argsText = argsList |> List.map (formatExpr syntax) |> String.concat ", "
                $"{funcText}({argsText})"
            | _ ->
                let funcText = parenthesizeIfNeeded funcExpr (formatExpr syntax funcExpr)
                if isUnitArgumentList args then
                    $"{funcText}()"
                else
                    let argsText = argsList |> formatInterpreterAppArgs |> String.concat " "
                    $"{funcText} {argsText}"
    | FuncRef funcName -> formatIdentifierPath funcName
    | Closure (funcName, captures) ->
        let capturesText = captures |> List.map (formatExpr syntax) |> String.concat ", "
        $"Closure({formatIdentifierPath funcName}, [{capturesText}])"

let private formatFunctionDef (syntax: Syntax) (funcDef: FunctionDef) : string =
    match syntax with
    | CompilerSyntax ->
        let typeParamsText =
            if List.isEmpty funcDef.TypeParams then ""
            else
                let joined = String.concat ", " funcDef.TypeParams
                $"<{joined}>"
        let paramsText =
            funcDef.Params
            |> NonEmptyList.toList
            |> (fun parameters ->
                if isSyntheticUnitParamList funcDef.Params then
                    ""
                else
                    parameters
                    |> List.map (fun (name, typ) -> $"{name}: {formatType typ}")
                    |> String.concat ", ")
        $"def {formatIdentifierSegment funcDef.Name}{typeParamsText}({paramsText}) : {formatType funcDef.ReturnType} = {formatExpr syntax funcDef.Body}"
    | InterpreterSyntax ->
        let typeParamsText =
            if List.isEmpty funcDef.TypeParams then ""
            else
                let joined = String.concat ", " funcDef.TypeParams
                $"<{joined}>"
        let paramsText =
            funcDef.Params
            |> NonEmptyList.toList
            |> (fun parameters ->
                if isSyntheticUnitParamList funcDef.Params then
                    ""
                else
                    parameters
                    |> List.map (fun (name, typ) -> $"{name}: {formatType typ}")
                    |> String.concat ", ")
        $"let {formatIdentifierSegment funcDef.Name}{typeParamsText}({paramsText}) : {formatType funcDef.ReturnType} = {formatExpr syntax funcDef.Body}"

let private formatTypeDef (typeDef: TypeDef) : string =
    let formatTypeParams (typeParams: string list) : string =
        if List.isEmpty typeParams then ""
        else
            let joined = String.concat ", " typeParams
            $"<{joined}>"

    match typeDef with
    | RecordDef (name, typeParams, fields) ->
        let fieldsText =
            fields
            |> List.map (fun (fieldName, fieldType) ->
                $"{formatIdentifierSegment fieldName}: {formatType fieldType}")
            |> String.concat ", "
        $"type {formatIdentifierSegment name}{formatTypeParams typeParams} = {{ {fieldsText} }}"
    | SumTypeDef (name, typeParams, variants) ->
        let variantsText =
            variants
            |> List.map (fun variant ->
                match variant.Payload with
                | None -> formatIdentifierSegment variant.Name
                | Some payloadType ->
                    $"{formatIdentifierSegment variant.Name} of {formatType payloadType}")
            |> String.concat " | "
        $"type {formatIdentifierSegment name}{formatTypeParams typeParams} = {variantsText}"
    | TypeAlias (name, typeParams, targetType) ->
        $"type {formatIdentifierSegment name}{formatTypeParams typeParams} = {formatType targetType}"

let private formatTopLevel (syntax: Syntax) (topLevel: TopLevel) : string =
    match topLevel with
    | FunctionDef funcDef -> formatFunctionDef syntax funcDef
    | TypeDef typeDef -> formatTypeDef typeDef
    | Expression expr -> formatExpr syntax expr

let formatProgram (syntax: Syntax) (Program items: Program) : string =
    let separator =
        match syntax with
        | CompilerSyntax -> "\n"
        | InterpreterSyntax -> "\n;\n"
    items |> List.map (formatTopLevel syntax) |> String.concat separator
