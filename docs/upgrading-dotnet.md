# Upgrading .NET Versions

To upgrade .NET versions, you will need to:

## Get it working locally

You'll need to make several updates to the `Dockerfile` at the root of this repo

- Update the `DOTNET_SDK_VERSION` environment variable set[1]

- Update the `dotnet_sha512` used when `curl`ing `dotnet.tar.gz` to the
  appropriate value

- Update the SDK version in `fsharp-backend/global.json` with the new SDK version[1]

[1] When updating to a preview/RC version, use the full version number
e.g. `7.0.100-preview.3.22179.4`

## Consider updating Paket

- dotnet-tools.json
- fsi-setup.fsx
- dotnet-regen-fsi

## Consider updating NuGet dependencies

- and NuGet dependencies

## Prepare for CI / Prod

follow instructions [here]() about updating the darklang dockerfile

- update the `github.com/darklang/dockerfile` repo to also reference the
  intended .NET SDK build number

- update `.circleci/config.yml` in two places