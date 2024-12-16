Relates to locally installing the `darklang` CLI tool
(as opposed to running it without any install).

To run the CLI executable against the local package manager:

From a command line set an environment variable

- On MacOS and Linux:
  `DARK_CONFIG_PACKAGE_MANAGER_BASE_URL=http://dark-packages.dlio.localhost:11001 [path]/darklang-alpha-[hash]-[os]-[arch] [command]`

- On Windows:
  `set DARK_CONFIG_PACKAGE_MANAGER_BASE_URL=http://dark-packages.dlio.localhost:11001`

Then run the CLI: `[path]/darklang-alpha-[hash]-[os]-[arch].exe`

Note:
you might have to add `127.0.0.1 dark-packages.dlio.localhost` to:

- `etc/hosts` if you are on Mac
- `C://Windows/System32/drivers/etc/hosts` on Windows
