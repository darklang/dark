CREATE TABLE IF NOT EXISTS item_docs (
               item_hash TEXT NOT NULL,
               part TEXT NOT NULL,
               within TEXT NOT NULL,
               text TEXT NOT NULL,
               origin_ts TEXT NOT NULL,
               PRIMARY KEY (item_hash, part, within));
