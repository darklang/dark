CREATE TABLE IF NOT EXISTS
system_migrations_v0
( name TEXT PRIMARY KEY
, execution_date TIMESTAMPTZ NOT NULL
, sql TEXT NOT NULL
);


CREATE TABLE IF NOT EXISTS
accounts_v0
( id UUID PRIMARY KEY
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

--------------------
-- Stuff that belongs in "package space"
--------------------
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
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS
package_constants_v0
-- IDs
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL
, owner TEXT NOT NULL
, modules TEXT NOT NULL /* eg Twitter.Other; includes package name, but not owner name */
, name TEXT NOT NULL /* eg pi */
, version INTEGER NOT NULL /* eg 0 */
-- the actual definition
, definition BYTEA NOT NULL /* the whole thing serialized as binary */
-- bonus
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

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
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);


--------------------
-- Stuff that belongs in "user space"
--------------------
CREATE TABLE IF NOT EXISTS
canvases_v0
( id UUID PRIMARY KEY
, account_id UUID REFERENCES accounts_v0(id) NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- HttpHandlers
CREATE TABLE IF NOT EXISTS
domains_v0
( domain TEXT PRIMARY KEY
, canvas_id UUID NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW());

-- TODO: HttpHandlers


-- Crons
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




-- Queues / Workers
CREATE TYPE scheduling_rule_type AS ENUM ('pause', 'block');

CREATE TABLE IF NOT EXISTS
scheduling_rules_v0
( id UUID PRIMARY KEY
, rule_type scheduling_rule_type NOT NULL
, canvas_id UUID NOT NULL
, handler_name TEXT NOT NULL
, event_space TEXT NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

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
ON
  queue_events_v0 (canvas_id, module, name);





CREATE TABLE IF NOT EXISTS
secrets_v0
( canvas_id UUID NOT NULL
, name VARCHAR(255) NOT NULL
, value TEXT NOT NULL
, version INT NOT NULL
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
, PRIMARY KEY (canvas_id, name, version) -- TODO: simplfy PK
);





CREATE TYPE toplevel_type AS
ENUM ('handler', 'db', 'user_function', 'user_type', 'user_constant');

CREATE TABLE IF NOT EXISTS
toplevels_v0
( canvas_id UUID NOT NULL
, tlid BIGINT NOT NULL
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


-- Keep track of what traces are available for which handler/function/etc
CREATE TABLE IF NOT EXISTS
traces_v0
( id UUID PRIMARY KEY
, canvas_id UUID NOT NULL
-- the handler's (or for a function's default trace, the function's) TLID (used to
-- store the trace data in Cloud Storage)
, root_tlid BIGINT NOT NULL
, trace_id UUID NOT NULL
, callgraph_tlids BIGINT[] NOT NULL -- functions called during the trace
);


-- "User DB" data
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
, CONSTRAINT user_data_key_uniq UNIQUE (canvas_id, table_tlid, dark_version, user_version, key)
);

CREATE INDEX IF NOT EXISTS
  idx_user_data_fetch
ON user_data_v0
  (canvas_id, table_tlid, user_version, dark_version);

CREATE INDEX IF NOT EXISTS
idx_user_data_current_data_for_tlid
ON user_data_v0
(user_version, dark_version, canvas_id, table_tlid);

CREATE INDEX IF NOT EXISTS
  idx_user_data_gin_data
ON user_data_v0
USING GIN
(data jsonb_path_ops)





-- the "Idea" of some "Pizza" that I'm modeling
-- many specific implementations of the "Pizza" idea
-- _the_ "current" Pizza




CREATE TABLE IF NOT EXISTS
package_type_declaration_v0
-- IDs
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL
, definition BYTEA NOT NULL 
, hash BYTEA NOT NULL --?
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS
package_type_declaration_v0
( id UUID PRIMARY KEY
, tlid BIGINT NOT NULL
, owner TEXT NOT NULL
, modules TEXT NOT NULL 
, typename TEXT NOT NULL 
--, version INTEGER NOT NULL 
, definition BYTEA NOT NULL 
, hash BYTEA NOT NULL
, dependencies_types BYTEA NOT NULL -- other type. also: not sure if we store only _immediate_ references, or also transitive refs
, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- type FQPackageTypeName =
-- { owner : string
--     // TODO: consider whether modules should be a NonEmptyList
--     modules : List<string>
--     name : string
--     version : int }


type PackageTypeDeclaration =
    {
        declaration : TypeDeclaration.T
        hash: ...
    }

type NamedPackageType =
    {
        name : FQTypeName.Package;
        hash: ...
    }







-- CREATE TABLE IF NOT EXISTS
-- named_package_type
-- ( id UUID PRIMARY KEY
-- , owner TEXT NOT NULL
-- , typename TEXT NOT NULL 

-- , current_modulespace
-- , current_hash
-- , is_current BOOL
-- , created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
-- );

-- CREATE TABLE IF NOT EXISTS
-- package_type_declaration_v0
-- ( id UUID PRIMARY KEY
-- , tlid BIGINT NOT NULL
-- , definition BYTEA NOT NULL 
-- , hash BYTEA NOT NULL
-- , dependencies_types BYTEA NOT NULL -- other type. also: not sure if we store only _immediate_ references, or also transitive refs
-- , created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
-- );
