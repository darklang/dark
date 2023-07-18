CREATE OR REPLACE FUNCTION trigger_set_timestamp()
RETURNS TRIGGER AS $t$
  BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
  END;
$t$ LANGUAGE plpgsql;


CREATE TABLE IF NOT EXISTS
accounts_v0
( id UUID PRIMARY KEY
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS
canvases_v0
( id UUID PRIMARY KEY
, account_id UUID REFERENCES accounts_v0(id) NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TRIGGER set_canvas_timestamp
BEFORE UPDATE ON canvases_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp();


CREATE TABLE IF NOT EXISTS
cron_records_v0
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL
, canvas_id UUID NOT NULL
, ran_at TIMESTAMPTZ NOT NULL DEFAULT NOW() -- default as it's cheap
);

CREATE INDEX IF NOT EXISTS
idx_cron_records_tlid_canvas_id_id
ON cron_records_v0
(tlid, canvas_id, id DESC);


CREATE TABLE IF NOT EXISTS
domains_v0
( domain TEXT PRIMARY KEY
, canvas_id UUID NOT NULL);


CREATE TABLE IF NOT EXISTS
queue_events_v0
( id UUID PRIMARY KEY
, canvas_id UUID NOT NULL
, module TEXT NOT NULL
, name TEXT NOT NULL
, modifier TEXT NOT NULL
, locked_at TIMESTAMPTZ -- nullable
, enqueued_at TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
);

-- We want to use this index to:
-- 1) count the number of items in this queue, so it's important that the entire
-- search term is in the index or it will need to hit disk. This is true even though
-- the module rarely changes
-- 2) fetch the indexes for all items we're unpausing. This is rare so it's fine to
CREATE INDEX IF NOT EXISTS
idx_queue_events_count
ON queue_events_v0 (canvas_id, module, name);


CREATE TABLE IF NOT EXISTS
package_functions_v0
-- IDs
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL -- includes TLID for tracing
  /* owner/namespace part of the string, eg dark.
   * CLEANUP This isn't a good way to store this because the username should be
   * stored in the editor canvas. But we haven't got all the details worked out so
   * for now store the owner */
-- allow search by name
, owner TEXT NOT NULL
, modules TEXT NOT NULL /* eg Twitter.Other; includes package name, but not owner name */
, fnname TEXT NOT NULL /* eg sendText */
, version INTEGER NOT NULL /* eg 0 */
-- the actual definition
, definition BYTEA NOT NULL /* the whole thing serialized as binary */
-- bonus
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TRIGGER set_package_function_timestamp
BEFORE UPDATE ON package_functions_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp();

CREATE TABLE IF NOT EXISTS
package_types_v0
-- IDs
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL -- includes TLID for tracing
  /* owner/namespace part of the string, eg dark.
   * CLEANUP This isn't a good way to store this because the username should be
   * stored in the editor canvas. But we haven't got all the details worked out so
   * for now store the owner */
-- allow search by name
, owner TEXT NOT NULL
, modules TEXT NOT NULL /* eg Twitter.Other; includes package name, but not owner name */
, typename TEXT NOT NULL /* eg sendText */
, version INTEGER NOT NULL /* eg 0 */
-- the actual definition
, definition BYTEA NOT NULL /* the whole thing serialized as binary */
-- bonus
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TRIGGER set_package_type_timestamp
BEFORE UPDATE ON package_types_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp();



CREATE TYPE scheduling_rule_type AS ENUM ('pause', 'block');

CREATE TABLE IF NOT EXISTS
scheduling_rules_v0
( id SERIAL PRIMARY KEY -- TODO: change to UUID
, rule_type scheduling_rule_type NOT NULL
, canvas_id UUID NOT NULL
, handler_name TEXT NOT NULL
, event_space TEXT NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);


CREATE TABLE IF NOT EXISTS
secrets_v0
( canvas_id UUID NOT NULL
, name VARCHAR(255) NOT NULL
, value TEXT NOT NULL
, version INT NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, PRIMARY KEY (canvas_id, name, version) -- TODO: simplfy PK
);




CREATE TABLE IF NOT EXISTS
system_migrations_v0
( name TEXT PRIMARY KEY
, execution_date TIMESTAMPTZ NOT NULL
, sql TEXT NOT NULL
);



CREATE TYPE toplevel_type AS
ENUM ('handler', 'db', 'user_function', 'user_tipe');

CREATE TABLE IF NOT EXISTS
toplevels_v0
( canvas_id UUID NOT NULL
, tlid INTEGER NOT NULL
, digest CHAR(32) NOT NULL
, tipe toplevel_type NOT NULL
, name TEXT /* handlers only - used for http lookups */
, module TEXT /* handlers only */
, modifier TEXT /* handlers only */
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, deleted BOOLEAN NOT NULL
, data BYTEA NOT NULL
, PRIMARY KEY (canvas_id, tlid)
);

CREATE TRIGGER set_toplevels_timestamp
BEFORE UPDATE ON toplevels_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp();



-- Keep track of what traces are available for which handler/function/etc
CREATE TABLE IF NOT EXISTS
traces_v0
( id UUID PRIMARY KEY DEFAULT gen_random_uuid()
, canvas_id UUID NOT NULL
-- the handler's (or for a function's default trace, the function's) TLID (used to
-- store the trace data in Cloud Storage)
, root_tlid BIGINT NOT NULL
, trace_id UUID NOT NULL
, callgraph_tlids BIGINT[] NOT NULL -- functions called during the trace
);


CREATE TABLE IF NOT EXISTS
user_data_v0
( id UUID PRIMARY KEY
, canvas_id UUID NOT NULL
, table_tlid BIGINT NOT NULL
, user_version INT NOT NULL
, dark_version INT NOT NULL
, data JSONB NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, key TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS
idx_user_data_fetch
ON user_data_v0
(canvas_id, table_tlid, user_version, dark_version);

CREATE INDEX IF NOT EXISTS
idx_user_data_current_data_for_tlid
ON user_data_v0
(user_version, dark_version, canvas_id, table_tlid);

CREATE UNIQUE INDEX
idx_user_data_row_uniqueness
ON user_data_v0
(canvas_id, table_tlid, dark_version, user_version, key);


CREATE INDEX IF NOT EXISTS
idx_user_data_gin_data
ON user_data_v0
USING GIN
(data jsonb_path_ops);

CREATE TRIGGER set_user_data_timestamp
BEFORE UPDATE ON user_data_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp();


ALTER TABLE user_data_v0 ADD constraint user_data_key_uniq UNIQUE USING INDEX idx_user_data_row_uniqueness;

CREATE TABLE IF NOT EXISTS
package_constants_v0
-- IDs
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL
, owner TEXT NOT NULL
, modules TEXT NOT NULL /* eg Twitter.Other; includes package name, but not owner name */
, fnname TEXT NOT NULL /* eg sendText */
, version INTEGER NOT NULL /* eg 0 */
-- the actual definition
, definition BYTEA NOT NULL /* the whole thing serialized as binary */
-- bonus
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TRIGGER set_package_constants_timestamp
BEFORE UPDATE ON package_constants_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
