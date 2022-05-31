-- These is for the the F# serialization, that will live alongside the old body for now.
ALTER TABLE packages_v0 ADD COLUMN IF NOT EXISTS body2 BYTEA