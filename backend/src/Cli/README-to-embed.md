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
2. The database file can be safely deleted - it will be recreated on next CLI run
3. For support, visit: https://darklang.com/support

## Moving the CLI

If you move the CLI executable, this directory should move with it to maintain your local package cache and settings. Otherwise, updates to your local package store will be lost.