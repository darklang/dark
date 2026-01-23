# Darklang VS Code Extension

This is a work-in-progress, not yet ready for general consumption.

Join [our Discord](https://darklang.com/discord-invite) to learn more.

## Features

- Syntax highlighting for `.dark` files
- Language server integration
- Darklang Classic color theme (optional)

## Darklang Classic Theme

The extension includes an optional color theme that matches the classic Darklang editor style. To use it:

1. Open VS Code Command Palette (Cmd/Ctrl+Shift+P)
2. Type "Color Theme"
3. Select "Darklang Classic" from the list

The theme features the classic dark color scheme with:
- Dark gray background (#282828)
- Purple-tinted strings (#c7abcd)
- Green keywords (#a1b56c)
- Blue accents for UI elements (#7cafc2)
- Orange operators (#dc9656)

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
