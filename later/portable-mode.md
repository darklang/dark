# Portable mode

<!-- WHEN: the no-portable-install pass, 2026-11-03 -->

Notes only; nothing here to put back. Every `dark` now keeps its data in `~/.darklang`
(`LibConfig.StoreLocation.defaultDirectory`), and an old portable store is neither read nor migrated.

## What it was

A published `dark` in `~/.darklang/bin` used `~/.darklang`. Anywhere else (a download folder, say)
it made a `.darklang` beside itself, so one person could have several stores. `DARK_HOME` (earlier
`DARK_CONFIG_RUNDIR`) overrode both; only tests and tooling set it. The README's manual install
("run it from wherever you put it") made every hand-installed `dark` portable.

## Where it lived

- `backend/src/Cli/EmbeddedResources.fs`: `exeDirectory`, `isInstalledMode`,
  `getDefaultDarklangDirectory`, `getDarklangDirectory` (`DARK_HOME` first).
- `backend/src/LibConfig/Config.fs`: `runDir` fell back to `absoluteDirOrCurrent "DARK_HOME"`, so a
  Debug build without it wrote `data.db` into the current directory.
- `packages/darklang/cli/installation/system.dark`: `InstallationMode`, `installationModeToString`,
  `getInstallationMode`, `migratePortableData`, `globalInstallationExists`,
  `installFromCurrentBinary` (its copy-or-download and copy-data-or-fresh prompts), `install`,
  `updateIfAvailable` (installed or portable `replace`).
- `install.dark` (`--uninstall-if-installed`, "Detected portable mode"), `update.dark` (the
  `Portable` case), `download.dark` (`updatePortableInPlace`; `smokeTest` relied on finding
  `.darklang` beside the copied binary), `uninstall.dark` (refused in portable mode), `status.dark`
  ("Running in portable mode"), `registry.dark` (`acceptsInstall`), all under
  `packages/darklang/cli/`.
- `scripts/build-and-install-cli-on-host` (`install --uninstall-if-installed`) and
  `.circleci/config.yml` (`rm -rf clis/.darklang` in `package-clis`).
- `DARK_HOME` users, now given their own HOME instead: `config/dev`, `config/circleci`, the gates
  and expect fixtures, `LibDB/TestProcess.fs`, `tests/Tests/CliInstance.fs`,
  `tests/cli/instance.dark`, `scripts/perf/*`, `scripts/run-backend-tests`,
  `validate-cli-artifact` and the `.circleci` Python tests.

## Migrating an old portable store, if we ever want to

1. At startup in `EmbeddedResources.extract`, once the store directory is chosen: if
   `<dir of Environment.ProcessPath>/.darklang/data.db` exists and is not `~/.darklang`, print one
   stderr line saying where it is and that `dark install` moves it.
2. `install` asks (`Cli.Prompt.confirmExplicit`; `--yes` accepts). It cannot move the store itself:
   this process already has `~/.darklang/data.db` open, and swapping a SQLite file under an open
   connection loses WAL pages. So it writes the old path to a marker, `~/.darklang/move-portable-store`,
   with `FileSystem.overwriteFile` (`writeFile` creates the folder, which the host refuses there).
3. The next start, before any store opens, moves every entry of the old folder except `bin` into
   `~/.darklang`, setting what it replaces aside in `~/.darklang/set-aside-<UTC time>/`, deletes the
   marker, and prints what moved. The usual startup reconcile then upgrades the moved store.
4. `Directory.Move` fails across filesystems, so it needs a copy-then-delete fallback.
