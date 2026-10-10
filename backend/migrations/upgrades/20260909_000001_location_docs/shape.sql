DROP TABLE IF EXISTS item_docs;
CREATE TABLE IF NOT EXISTS location_docs (
               owner TEXT NOT NULL,
               modules TEXT NOT NULL,
               name TEXT NOT NULL,
               kind TEXT NOT NULL,
               within TEXT NOT NULL,
               text TEXT NOT NULL,
               origin_ts TEXT NOT NULL,
               PRIMARY KEY (owner, modules, name, kind, within));
