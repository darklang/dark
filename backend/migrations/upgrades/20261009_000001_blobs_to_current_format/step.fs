/// The `20261009_000001_blobs_to_current_format` upgrade; why it exists is in `upgrade.toml` beside this file.
module LibDB.UpgradeSteps.U20261009_000001_blobs_to_current_format

let run () : unit = LibDB.Upgrades.rewriteBlobsToCurrentFormat ()
