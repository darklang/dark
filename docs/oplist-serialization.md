# Versioning oplists

Oplists are versioned by the hash of the "shape" of their structure.
If you change the structure of an Op then the binary version will no longer load.

If you make a change to anything with uses `[@@deriving bin_io]`, you need to make sure of the following:

- if you add a new sum type constructor
  - put new constructors at the bottom
  - if there are exactly 256 constructors, it'll change the size and you need
    to do a migration.

- if you change a sum type constructor
  - name changes are ok
  - definitely do not change the type of a constructor

- if you are changing a record or a tuple:
  - I'm not sure what's safe here. See the bin_prot docs listed below.

- serialization file:
  - commit the serialization file (backend/serializations/someSha)

- migrations:
  - download the prod DB using prodclone, and check whether you can load the
    oplists, using http://localhost:8000/ops/check-all-canvases
    - This will check it can read them all
    - This will also convert them from the old format to the new format.

  - The toplevel_oplists table should only have one digest using psql:
    - `select distinct digest from toplevel_oplists;`

  - If you change the format of the oplists in a weird way, you need to write a
    migration. Check git history for how to do it.

  - We automatically validate the oplists for important canvases on deployment.
    See Libbackend.Serialize.tier_one_hosts

  - After deploying a serialization change, go through the conversion process
    to bring all oplists to the latest version. Just go to
    https://builtwithdark.com/ops/check-all-canvases and click the button.



# Bin_prot

The binary serialization format we use is bin-prot, made by Jane Street. It is
extremely fast, and quite small.

Read about it here: https://github.com/janestreet/bin_prot
