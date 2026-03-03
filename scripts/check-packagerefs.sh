#!/bin/bash
# Fetches current hashes from data.db for all locations referenced by PackageRefs.fs.
# Compare output against PackageRefs.fs to find what needs updating.
DB="${1:-rundir/data.db}"

sqlite3 "$DB" << 'SQL'
.mode column
.headers on
SELECT modules, name, item_hash, item_type
FROM locations
WHERE owner = 'Darklang'
  AND deprecated_at IS NULL
  AND (modules, name, item_type) IN (
    -- Type.Stdlib
    ('Stdlib.Result', 'Result', 'type'),
    ('Stdlib.Option', 'Option', 'type'),
    ('Stdlib.Int8', 'ParseError', 'type'),
    ('Stdlib.UInt8', 'ParseError', 'type'),
    ('Stdlib.Int16', 'ParseError', 'type'),
    ('Stdlib.UInt16', 'ParseError', 'type'),
    ('Stdlib.Int32', 'ParseError', 'type'),
    ('Stdlib.UInt32', 'ParseError', 'type'),
    ('Stdlib.Int64', 'ParseError', 'type'),
    ('Stdlib.UInt64', 'ParseError', 'type'),
    ('Stdlib.Int128', 'ParseError', 'type'),
    ('Stdlib.UInt128', 'ParseError', 'type'),
    ('Stdlib.Float', 'ParseError', 'type'),
    ('Stdlib.Uuid', 'ParseError', 'type'),
    ('Stdlib.Http', 'Request', 'type'),
    ('Stdlib.Http', 'Response', 'type'),
    ('Stdlib.HttpClient', 'BadHeader', 'type'),
    ('Stdlib.HttpClient', 'BadUrlDetails', 'type'),
    ('Stdlib.HttpClient', 'RequestError', 'type'),
    ('Stdlib.HttpClient', 'Response', 'type'),
    ('Stdlib.Json.ParseError.JsonPath.Part', 'Part', 'type'),
    ('Stdlib.Json.ParseError', 'ParseError', 'type'),
    ('Stdlib.AltJson.ParseError', 'ParseError', 'type'),
    ('Stdlib.AltJson', 'Json', 'type'),
    ('Stdlib.Cli', 'ExecutionOutcome', 'type'),
    ('Stdlib.Cli.OS', 'OS', 'type'),
    ('Stdlib.Cli.Stdin.Modifiers', 'Modifiers', 'type'),
    ('Stdlib.Cli.Stdin.Key', 'Key', 'type'),
    ('Stdlib.Cli.Stdin.KeyRead', 'KeyRead', 'type'),

    -- Type.Builtins
    ('Builtins', 'Purity', 'type'),
    ('Builtins', 'ParamInfo', 'type'),
    ('Builtins', 'FunctionInfo', 'type'),

    -- Type.LanguageTools
    ('LanguageTools', 'Sign', 'type'),
    ('LanguageTools', 'BuiltinValue', 'type'),
    ('LanguageTools', 'BuiltinFunctionParameter', 'type'),
    ('LanguageTools', 'BuiltinFunction', 'type'),
    ('LanguageTools.Parser', 'Point', 'type'),
    ('LanguageTools.Parser', 'Range', 'type'),
    ('LanguageTools.Parser', 'ParsedNode', 'type'),
    ('LanguageTools.Parser.CliScript', 'PTCliScriptModule', 'type'),

    -- Type.LanguageTools.RuntimeTypes
    ('LanguageTools.RuntimeTypes', 'Hash', 'type'),
    ('LanguageTools.RuntimeTypes.FQTypeName', 'Package', 'type'),
    ('LanguageTools.RuntimeTypes.FQTypeName', 'FQTypeName', 'type'),
    ('LanguageTools.RuntimeTypes.FQValueName', 'Builtin', 'type'),
    ('LanguageTools.RuntimeTypes.FQValueName', 'FQValueName', 'type'),
    ('LanguageTools.RuntimeTypes.FQFnName', 'Builtin', 'type'),
    ('LanguageTools.RuntimeTypes.FQFnName', 'FQFnName', 'type'),
    ('LanguageTools.RuntimeTypes', 'NameResolutionError', 'type'),
    ('LanguageTools.RuntimeTypes', 'NameResolution', 'type'),
    ('LanguageTools.RuntimeTypes', 'TypeReference', 'type'),
    ('LanguageTools.RuntimeTypes', 'LetPattern', 'type'),
    ('LanguageTools.RuntimeTypes', 'MatchPattern', 'type'),
    ('LanguageTools.RuntimeTypes', 'StringSegment', 'type'),
    ('LanguageTools.RuntimeTypes', 'Dval', 'type'),
    ('LanguageTools.RuntimeTypes', 'KnownType', 'type'),
    ('LanguageTools.RuntimeTypes', 'ValueType', 'type'),
    ('LanguageTools.RuntimeTypes', 'ApplicableNamedFn', 'type'),
    ('LanguageTools.RuntimeTypes', 'ApplicableLambda', 'type'),
    ('LanguageTools.RuntimeTypes', 'Applicable', 'type'),

    -- Type.LanguageTools.RuntimeTypes.RuntimeError
    ('LanguageTools.RuntimeTypes.RuntimeError.Bools', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Ints', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Strings', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Lists', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Dicts', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Lets', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Matches', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Enums', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Records', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Applications', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Statements', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Unwraps', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.Jsons', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError.CLIs', 'Error', 'type'),
    ('LanguageTools.RuntimeTypes.RuntimeError', 'Error', 'type'),

    -- Type.LanguageTools.ProgramTypes
    ('LanguageTools.ProgramTypes', 'Hash', 'type'),
    ('LanguageTools.ProgramTypes', 'NameResolutionError', 'type'),
    ('LanguageTools.ProgramTypes', 'NameResolution', 'type'),
    ('LanguageTools.ProgramTypes.FQTypeName', 'Package', 'type'),
    ('LanguageTools.ProgramTypes.FQTypeName', 'FQTypeName', 'type'),
    ('LanguageTools.ProgramTypes.FQValueName', 'Builtin', 'type'),
    ('LanguageTools.ProgramTypes.FQValueName', 'FQValueName', 'type'),
    ('LanguageTools.ProgramTypes.FQFnName', 'Builtin', 'type'),
    ('LanguageTools.ProgramTypes.FQFnName', 'FQFnName', 'type'),
    ('LanguageTools.ProgramTypes', 'TypeReference', 'type'),
    ('LanguageTools.ProgramTypes', 'LetPattern', 'type'),
    ('LanguageTools.ProgramTypes', 'MatchPattern', 'type'),
    ('LanguageTools.ProgramTypes', 'MatchCase', 'type'),
    ('LanguageTools.ProgramTypes', 'StringSegment', 'type'),
    ('LanguageTools.ProgramTypes', 'BinaryOperation', 'type'),
    ('LanguageTools.ProgramTypes', 'InfixFnName', 'type'),
    ('LanguageTools.ProgramTypes', 'Infix', 'type'),
    ('LanguageTools.ProgramTypes', 'PipeExpr', 'type'),
    ('LanguageTools.ProgramTypes', 'Expr', 'type'),
    ('LanguageTools.ProgramTypes', 'Deprecation', 'type'),
    ('LanguageTools.ProgramTypes', 'PackageLocation', 'type'),
    ('LanguageTools.ProgramTypes', 'LocatedItem', 'type'),
    ('LanguageTools.ProgramTypes.TypeDeclaration', 'RecordField', 'type'),
    ('LanguageTools.ProgramTypes.TypeDeclaration', 'EnumField', 'type'),
    ('LanguageTools.ProgramTypes.TypeDeclaration', 'EnumCase', 'type'),
    ('LanguageTools.ProgramTypes.TypeDeclaration', 'Definition', 'type'),
    ('LanguageTools.ProgramTypes.TypeDeclaration', 'TypeDeclaration', 'type'),
    ('LanguageTools.ProgramTypes.PackageType', 'PackageType', 'type'),
    ('LanguageTools.ProgramTypes.PackageValue', 'PackageValue', 'type'),
    ('LanguageTools.ProgramTypes.PackageFn', 'Parameter', 'type'),
    ('LanguageTools.ProgramTypes.PackageFn', 'PackageFn', 'type'),
    ('LanguageTools.ProgramTypes.Search', 'EntityType', 'type'),
    ('LanguageTools.ProgramTypes.Search', 'SearchDepth', 'type'),
    ('LanguageTools.ProgramTypes.Search', 'SearchQuery', 'type'),
    ('LanguageTools.ProgramTypes.Search', 'SearchResults', 'type'),
    ('LanguageTools.ProgramTypes', 'PackageOp', 'type'),
    ('LanguageTools.ProgramTypes', 'ItemKind', 'type'),
    ('LanguageTools.ProgramTypes', 'PropagateRepoint', 'type'),
    ('LanguageTools.ProgramTypes', 'Secret', 'type'),
    ('LanguageTools.ProgramTypes', 'DB', 'type'),
    ('LanguageTools.ProgramTypes.Handler', 'CronInterval', 'type'),
    ('LanguageTools.ProgramTypes.Handler', 'Spec', 'type'),
    ('LanguageTools.ProgramTypes.Handler', 'Handler', 'type'),

    -- Type.PrettyPrinter
    ('PrettyPrinter.RuntimeTypes.RuntimeError', 'ErrorMessage', 'type'),

    -- Type.Cli
    ('Cli.ExecutionError', 'ExecutionError', 'type'),
    ('Cli.Scripts', 'Script', 'type'),

    -- Type.DarkPackages
    ('DarkPackages', 'Stats', 'type'),

    -- Type.SCM
    ('SCM.Branch', 'Branch', 'type'),
    ('SCM.Merge', 'MergeError', 'type'),
    ('SCM.PackageOps', 'Commit', 'type'),

    -- Type.Internal
    ('Internal.Canvas', 'Program', 'type'),
    ('Internal.Infra', 'TableSize', 'type'),

    -- Fn.Stdlib
    ('Stdlib.HttpClient', 'request', 'fn'),

    -- Fn.LanguageTools
    ('LanguageTools.Parser.TestParsing', 'parsePTExpr', 'fn'),
    ('LanguageTools.Parser.TestParsing', 'parsePTSourceFileWithOps', 'fn'),
    ('LanguageTools.Parser.CliScript', 'parseForCli', 'fn'),

    -- Fn.PrettyPrinter
    ('PrettyPrinter.RuntimeTypes', 'dval', 'fn'),
    ('PrettyPrinter.RuntimeTypes', 'fnName', 'fn'),
    ('PrettyPrinter.RuntimeTypes', 'typeReference', 'fn'),
    ('PrettyPrinter.RuntimeTypes.Dval', 'valueTypeName', 'fn'),
    ('PrettyPrinter.RuntimeTypes.RuntimeError', 'toString', 'fn'),
    ('PrettyPrinter.RuntimeTypes.RuntimeError', 'toErrorMessage', 'fn'),
    ('PrettyPrinter.ProgramTypes', 'sourceFile', 'fn'),

    -- Fn.Cli
    ('Cli', 'executeCliCommand', 'fn'),

    -- Fn.Internal
    ('Internal.Test', 'parseSingleTestFromFile', 'fn')
  )
ORDER BY item_type, modules, name;
SQL
