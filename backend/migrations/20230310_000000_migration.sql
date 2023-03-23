CREATE OR REPLACE FUNCTION trigger_set_timestamp()
RETURNS TRIGGER AS $t$
  BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
  END;
$t$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION canvas_id(_new_id uuid, _account_id uuid, _name VARCHAR(40), OUT _id uuid) AS
  $func$
  BEGIN
  LOOP
    SELECT id
    FROM   canvases_v0
    WHERE  name = _name
    INTO   _id;

    EXIT WHEN FOUND;

    INSERT INTO canvases_v0 AS c
    (id, account_id, name)
    VALUES (_new_id, _account_id, _name)
    ON     CONFLICT (name) DO NOTHING
    RETURNING c.id
    INTO   _id;

    EXIT WHEN FOUND;
  END LOOP;
  END;
$func$ LANGUAGE plpgsql;


CREATE TABLE IF NOT EXISTS
accounts_v0
( id UUID PRIMARY KEY
, username VARCHAR(255) UNIQUE NOT NULL
);

CREATE TRIGGER set_account_timestamp
BEFORE UPDATE ON accounts_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp();


CREATE TABLE IF NOT EXISTS
canvases_v0
( id UUID PRIMARY KEY
, account_id UUID REFERENCES accounts_v0(id) NOT NULL
, name VARCHAR(64) UNIQUE NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TRIGGER set_canvas_timestamp
BEFORE UPDATE ON canvases_v0
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp();


CREATE TABLE IF NOT EXISTS
cron_records_v0
( id SERIAL PRIMARY KEY
, tlid BIGINT NOT NULL
, canvas_id UUID NOT NULL
, ran_at TIMESTAMPTZ NOT NULL DEFAULT NOW() --remove default
);

CREATE INDEX IF NOT EXISTS
idx_cron_records_tlid_canvas_id_id
ON cron_records_v0
(tlid, canvas_id, id DESC);


CREATE TABLE IF NOT EXISTS
custom_domains_v0
( host TEXT PRIMARY KEY
, canvas TEXT);


CREATE TABLE IF NOT EXISTS
events_v0
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
idx_events_count
ON events_v0 (canvas_id, module, name);


CREATE TABLE IF NOT EXISTS
function_arguments_v0
( id UUID PRIMARY KEY DEFAULT gen_random_uuid()
, canvas_id UUID NOT NULL
, tlid BIGINT NOT NULL
, trace_id UUID NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, arguments_json TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS
function_arguments_most_recent
ON function_arguments_v0
(canvas_id, tlid, timestamp DESC);

CREATE INDEX IF NOT EXISTS
function_arguments_for_trace
ON function_arguments_v0
(canvas_id, tlid, trace_id);

-- TODO: add PK to function_results_v0
CREATE TABLE IF NOT EXISTS
function_results_v0
( id BIGINT NOT NULL
, canvas_id UUID NOT NULL
, tlid BIGINT NOT NULL
, fnname TEXT NOT NULL
, hash TEXT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
, trace_id UUID NOT NULL
, hash_version INTEGER NOT NULL
);

CREATE INDEX IF NOT EXISTS
idx_function_results_v0_most_recent
ON function_results_v0
(canvas_id, trace_id, tlid, timestamp DESC);



CREATE TABLE IF NOT EXISTS
 op_ctrs_v0
 ( canvas_id UUID NOT NULL
 , browser_id UUID NOT NULL UNIQUE
 , ctr INTEGER NOT NULL DEFAULT 0
 , timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW());

 CREATE INDEX IF NOT EXISTS
 idx_op_ctrs_canvas_id
 ON op_ctrs_v0
 (canvas_id);

 CREATE INDEX IF NOT EXISTS
 idx_op_ctrs_browser_id_ctr
 ON op_ctrs_v0
 (browser_id, ctr DESC);

 CREATE INDEX IF NOT EXISTS
 idx_op_ctrs_timestamp
 ON op_ctrs_v0
 (timestamp ASC);


 /* associate it back to the function. */
CREATE TABLE IF NOT EXISTS
packages_v0
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL
  /* owner/namespace part of the string, eg dark */
, user_id UUID NOT NULL
, package TEXT NOT NULL /* eg stdlib */
, module TEXT NOT NULL /* eg Twitter */
, fnname TEXT NOT NULL /* eg sendText */
, version INTEGER NOT NULL /* eg 0 */
, description TEXT NOT NULL /* docstring */
, body BYTEA NOT NULL
, return_type TEXT NOT NULL
, parameters jsonb NOT NULL
, author_id UUID NOT NULL /* who uploaded this */
, deprecated BOOL NOT NULL
, updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);


CREATE TYPE scheduling_rule_type AS ENUM ('pause', 'block');

CREATE TABLE IF NOT EXISTS
scheduling_rules_v0
( id SERIAL PRIMARY KEY -- TODO: change to UUID ERR: Can't cast database type uuid to Int32
, rule_type scheduling_rule_type NOT NULL
, canvas_id UUID NOT NULL
, handler_name TEXT NOT NULL
, event_space TEXT NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);


CREATE TABLE IF NOT EXISTS
secrets_v0
( canvas_id UUID NOT NULL
, secret_name VARCHAR(255) NOT NULL
, secret_value TEXT NOT NULL
, secret_version INT NOT NULL DEFAULT 0
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, PRIMARY KEY (canvas_id, secret_name, secret_version) -- TODO: simplfy PK
);


CREATE TABLE IF NOT EXISTS
stored_events_v0
( canvas_id UUID NOT NULL
, module TEXT NOT NULL
, path TEXT NOT NULL
, modifier TEXT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
, trace_id UUID NOT NULL
);

CREATE INDEX IF NOT EXISTS
idx_stored_events_v0_most_recent
ON stored_events_v0
(canvas_id, module, path, modifier, timestamp DESC);

CREATE INDEX IF NOT EXISTS
stored_events_v0_traceid
ON stored_events_v0
(canvas_id, trace_id);

CREATE INDEX IF NOT EXISTS
idx_stored_events_v0_most_recent_with_text
ON stored_events_v0
(canvas_id, module, path text_pattern_ops, modifier, "timestamp" DESC);



CREATE TABLE IF NOT EXISTS
system_migrations_v0
( name TEXT PRIMARY KEY
, execution_date TIMESTAMPTZ NOT NULL
, sql TEXT NOT NULL
);



CREATE TYPE toplevel_type AS
ENUM ('handler', 'db', 'user_function', 'user_tipe');

CREATE TABLE IF NOT EXISTS
toplevel_oplists_v0
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
, oplist BYTEA NOT NULL
, oplist_cache BYTEA NOT NULL
, PRIMARY KEY (canvas_id, tlid)
);

CREATE TRIGGER set_toplevel_oplist_timestamp
BEFORE UPDATE ON toplevel_oplists_v0
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

UPDATE user_data_v0 SET key = id::text;

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


ALTER TABLE user_data_v0 ADD constraint user_data_key_uniq UNIQUE USING INDEX idx_user_data_row_uniqueness