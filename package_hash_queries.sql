-- to check PackageIDs to delete when things are stable
-- queries for Result and Option
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Result';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Option';

-- ParseError queries for numeric types
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Int8';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.UInt8';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Int16';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.UInt16';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Int32';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.UInt32';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Int64';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.UInt64';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Int128';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.UInt128';

-- Float and Uuid ParseError queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Float';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Uuid';

-- Http types queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Request' AND modules = 'Stdlib.Http';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Response' AND modules = 'Stdlib.Http';

-- HttpClient types queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'BadHeader' AND modules = 'Stdlib.HttpClient';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'BadUrlDetails' AND modules = 'Stdlib.HttpClient';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'RequestError' AND modules = 'Stdlib.HttpClient';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Response' AND modules = 'Stdlib.HttpClient';

-- Json.ParseError types queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Part' AND modules = 'Stdlib.Json.ParseError.JsonPath.Part';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'JsonPath' AND modules = 'Stdlib.Json.ParseError.JsonPath';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.Json.ParseError';

-- AltJson types queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParseError' AND modules = 'Stdlib.AltJson.ParseError';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Json' AND modules = 'Stdlib.AltJson';

-- Cli types queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ExecutionOutcome' AND modules = 'Stdlib.Cli';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'OS' AND modules = 'Stdlib.Cli.OS';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Modifiers' AND modules = 'Stdlib.Cli.Stdin.Modifiers';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Key' AND modules = 'Stdlib.Cli.Stdin.Key';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'KeyRead' AND modules = 'Stdlib.Cli.Stdin.KeyRead';

-- LanguageTools types queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Sign' AND modules = 'LanguageTools';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'BuiltinValue' AND modules = 'LanguageTools';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'BuiltinFunctionParameter' AND modules = 'LanguageTools';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'BuiltinFunction' AND modules = 'LanguageTools';

-- LanguageTools.Parser types queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Point' AND modules = 'LanguageTools.Parser';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Range' AND modules = 'LanguageTools.Parser';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ParsedNode' AND modules = 'LanguageTools.Parser';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'PTCliScriptModule' AND modules = 'LanguageTools.Parser.CliScript';

-- LanguageTools other modules
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'OnMissing' AND modules = 'LanguageTools.NameResolver';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Name' AND modules = 'LanguageTools.WrittenTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Range' AND modules = 'LanguageTools.WrittenTypes';

-- LanguageTools.RuntimeTypes queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Package' AND modules = 'LanguageTools.RuntimeTypes.FQTypeName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'FQTypeName' AND modules = 'LanguageTools.RuntimeTypes.FQTypeName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Package' AND modules = 'LanguageTools.RuntimeTypes.FQValueName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Builtin' AND modules = 'LanguageTools.RuntimeTypes.FQValueName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'FQValueName' AND modules = 'LanguageTools.RuntimeTypes.FQValueName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Package' AND modules = 'LanguageTools.RuntimeTypes.FQFnName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Builtin' AND modules = 'LanguageTools.RuntimeTypes.FQFnName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'FQFnName' AND modules = 'LanguageTools.RuntimeTypes.FQFnName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'NameResolutionError' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'NameResolution' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'TypeReference' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'LetPattern' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'MatchPattern' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Dval' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'KnownType' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ValueType' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ApplicableNamedFn' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ApplicableLambda' AND modules = 'LanguageTools.RuntimeTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Applicable' AND modules = 'LanguageTools.RuntimeTypes';

-- LanguageTools.RuntimeTypes.RuntimeError queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'TypeCheckPathPart' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.TypeChecking';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ReverseTypeCheckPath' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.TypeChecking';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Bools';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Ints';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Strings';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Lists';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Dicts';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Lets';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Matches';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Enums';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Records';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Applications';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Statements';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Unwraps';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.Jsons';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError.CLIs';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Error' AND modules = 'LanguageTools.RuntimeTypes.RuntimeError';

-- LanguageTools.ProgramTypes queries (FIXED)
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'NameResolutionError' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Package' AND modules = 'LanguageTools.ProgramTypes.FQTypeName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'FQTypeName' AND modules = 'LanguageTools.ProgramTypes.FQTypeName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Builtin' AND modules = 'LanguageTools.ProgramTypes.FQValueName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Package' AND modules = 'LanguageTools.ProgramTypes.FQValueName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'FQValueName' AND modules = 'LanguageTools.ProgramTypes.FQValueName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Builtin' AND modules = 'LanguageTools.ProgramTypes.FQFnName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Package' AND modules = 'LanguageTools.ProgramTypes.FQFnName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'FQFnName' AND modules = 'LanguageTools.ProgramTypes.FQFnName';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'TypeReference' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'LetPattern' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'MatchPattern' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'MatchCase' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'StringSegment' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'BinaryOperation' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'InfixFnName' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Infix' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'PipeExpr' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Expr' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Deprecation' AND modules = 'LanguageTools.ProgramTypes';

-- LanguageTools.ProgramTypes.TypeDeclaration queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'RecordField' AND modules = 'LanguageTools.ProgramTypes.TypeDeclaration';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'EnumField' AND modules = 'LanguageTools.ProgramTypes.TypeDeclaration';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'EnumCase' AND modules = 'LanguageTools.ProgramTypes.TypeDeclaration';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Definition' AND modules = 'LanguageTools.ProgramTypes.TypeDeclaration';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'TypeDeclaration' AND modules = 'LanguageTools.ProgramTypes.TypeDeclaration';

-- LanguageTools.ProgramTypes.PackageType queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Name' AND modules = 'LanguageTools.ProgramTypes.PackageType';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'PackageType' AND modules = 'LanguageTools.ProgramTypes.PackageType';

-- LanguageTools.ProgramTypes.PackageValue queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Name' AND modules = 'LanguageTools.ProgramTypes.PackageValue';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'PackageValue' AND modules = 'LanguageTools.ProgramTypes.PackageValue';

-- LanguageTools.ProgramTypes.PackageFn queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Name' AND modules = 'LanguageTools.ProgramTypes.PackageFn';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Parameter' AND modules = 'LanguageTools.ProgramTypes.PackageFn';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'PackageFn' AND modules = 'LanguageTools.ProgramTypes.PackageFn';

-- LanguageTools.ProgramTypes.Search queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'EntityType' AND modules = 'LanguageTools.ProgramTypes.Search';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'SearchDepth' AND modules = 'LanguageTools.ProgramTypes.Search';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'SearchQuery' AND modules = 'LanguageTools.ProgramTypes.Search';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'SearchResults' AND modules = 'LanguageTools.ProgramTypes.Search';

-- LanguageTools.ProgramTypes Secret and DB queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Secret' AND modules = 'LanguageTools.ProgramTypes';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'DB' AND modules = 'LanguageTools.ProgramTypes';

-- LanguageTools.ProgramTypes.Handler queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'CronInterval' AND modules = 'LanguageTools.ProgramTypes.Handler';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Spec' AND modules = 'LanguageTools.ProgramTypes.Handler';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Handler' AND modules = 'LanguageTools.ProgramTypes.Handler';

-- PrettyPrinter queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ErrorMessage' AND modules = 'PrettyPrinter.RuntimeTypes.RuntimeError';

-- Cli queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'ExecutionError' AND modules = 'Cli.ExecutionError';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Script' AND modules = 'Cli.Scripts';

-- DarkPackages queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Stats' AND modules = 'DarkPackages';

-- Internal.Canvas queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Program' AND modules = 'Internal.Canvas';
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'Secret' AND modules = 'Internal.Canvas';

-- Internal.Infra queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'TableSize' AND modules = 'Internal.Infra';

-- Internal.Worker queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'SchedulingRule' AND modules = 'Internal.Worker';

-- Internal.Test queries
SELECT owner, modules, name, hash FROM package_types_v0 WHERE name = 'PTTest' AND modules = 'Internal.Test';

-- FUNCTION QUERIES

-- Stdlib function queries
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'map' AND modules = 'Stdlib.List';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'request' AND modules = 'Stdlib.HttpClient';

-- LanguageTools function queries
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'resolve' AND modules = 'LanguageTools.NameResolver.FnName';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'parsePTExpr' AND modules = 'LanguageTools.Parser.TestParsing';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'parseAndPrettyPrint' AND modules = 'LanguageTools.Parser.TestParsing';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'parse' AND modules = 'LanguageTools.Parser.CliScript';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'pm' AND modules = 'LanguageTools.PackageManager';

-- PrettyPrinter function queries
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'expr' AND modules = 'PrettyPrinter.ProgramTypes';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'toString' AND modules = 'PrettyPrinter.RuntimeTypes.RuntimeError';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'toErrorMessage' AND modules = 'PrettyPrinter.RuntimeTypes.RuntimeError';
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'fullForReference' AND modules = 'PrettyPrinter.ProgramTypes.FQFnName';

-- Cli function queries
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'executeCliCommand' AND modules = 'Cli';

-- Internal.Test function queries
SELECT owner, modules, name, hash FROM package_functions_v0 WHERE name = 'parseSingleTestFromFile' AND modules = 'Internal.Test';