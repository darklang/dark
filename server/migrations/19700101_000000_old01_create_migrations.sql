CREATE TABLE IF NOT EXISTS
migrations
( id BIGINT
, host TEXT
, sql TEXT
, PRIMARY KEY (id, host))
