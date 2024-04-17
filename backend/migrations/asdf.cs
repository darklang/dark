



We need types, function, and const _definitions_
and we need _names_ that refer to those definitions


type FQPackageTypeName =
{ owner : string
    // TODO: consider whether modules should be a NonEmptyList
    modules : List<string>
    name : string
    version : int }


type PackageTypeDeclaration =
    {
        declaration : TypeDeclaration.T
        hash: ...
    }

type NamedPackageType =
    {
        name : FQTypeName.Package;
hash: ...
    }