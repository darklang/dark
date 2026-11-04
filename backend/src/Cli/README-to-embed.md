# Darklang CLI Data Directory

This directory contains data files used by the Darklang CLI.

## Contents

- `data.db` - SQLite database containing package definitions and other CLI data
- `README.md` - This file explaining the directory contents
- `logs/` - CLI operation logs (created as needed)

## About this directory

This `.darklang` directory serves as the CLI's data directory. It contains:

- **Package data**: The SQLite database holds all the standard library and package definitions
- **Configuration**: Settings and cached data for faster CLI startup
- **Logs**: Operation logs for debugging and monitoring

## Troubleshooting

If you experience issues with the CLI:

1. Check the logs in the `logs/` subdirectory for error messages
2. Deleting `data.db` starts you over with a fresh store, losing what you made; `dark backups` lists copies
3. For support, visit: https://darklang.com/support

## One per user account

Every `dark` you run on this account keeps its data here, wherever the binary itself is. `dark install`
puts the CLI in `bin/` and on your PATH; `dark uninstall` removes this whole directory.
