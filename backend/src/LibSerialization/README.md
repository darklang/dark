# LibSerialization

How we store code in the DB.
It's very important to maintain this as PT and RT changes.

TODO figure out how to properly handle schema migrations...

The following changes are likely safe, and require very minor rework
- removing a variant at the end of an Enum (so long as that variant is not used in saved data)
- renaming a variant in an Enum (even if that variant is used)
- rename a field in a record (does not have to be the last field, don't change the keys of other fields)
- remove a field from a record (do not change the index of the other fields)
- adding a new variant at the end of an Enum

The following changes are likely unsafe (and would require migrating data):
- adding a new variant to an Enum that is not at the end
- removing a variant in an Enum that is not at the end
- reorder variants in an Enum

The following changes have not been tested but are assumed to be unsafe:
- adding a field to variant (eg add b to X(a,b))
- add a field to a record
- change the type of a field in a variant
- change the type of a field in a record
- removing a field from a variant (eg remove b to X(a,b))