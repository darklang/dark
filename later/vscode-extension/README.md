# Darklang VS Code Extension

This is a work-in-progress, not yet ready for general consumption.

Join [our Discord](https://darklang.com/discord-invite) to learn more.

## Running it

- (run whole repo in devcontainer -- see root README)
- `cd` to this dir
- `npm i`
- hit F5

## Publishing

The extension is automatically published to the VS Code Marketplace when:

1. The version in [`package.json`](./package.json) is bumped
2. The change is merged to the `main` branch

The CI pipeline compares the package.json version with the marketplace version and only publishes if the package.json version is newer.
