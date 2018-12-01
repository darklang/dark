CREATE TABLE IF NOT EXISTS
oplists
( host VARCHAR(64)
, digest CHAR(32)
, data BYTEA
, PRIMARY KEY (host, digest))
