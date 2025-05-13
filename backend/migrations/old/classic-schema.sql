
CREATE TYPE public.queue_status AS ENUM (
    'new',
    'locked',
    'done',
    'error',
    'scheduled',
    'missing'
);

CREATE TYPE public.scheduling_rule_type AS ENUM (
    'pause',
    'block'
);

CREATE TYPE public.toplevel_type AS ENUM (
    'handler',
    'db',
    'user_function',
    'user_tipe'
);

CREATE FUNCTION public.canvas_id(_new_id uuid, _account_id uuid, _name character varying, OUT _id uuid) RETURNS uuid
    LANGUAGE plpgsql
    AS $$
  BEGIN
  LOOP
    SELECT id
    FROM   canvases
    WHERE  name = _name
    INTO   _id;

    EXIT WHEN FOUND;

    INSERT INTO canvases AS c
    (id, account_id, name)
    VALUES (_new_id, _account_id, _name)
    ON     CONFLICT (name) DO NOTHING
    RETURNING c.id
    INTO   _id;

    EXIT WHEN FOUND;
  END LOOP;
  END;
$$;



CREATE FUNCTION public.trigger_set_timestamp() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
  END;
$$;



SET default_tablespace = '';

SET default_with_oids = false;

CREATE TABLE public.access (
    access_account uuid NOT NULL,
    organization_account uuid NOT NULL,
    permission character varying(255) NOT NULL
);




CREATE TABLE public.accounts (
    id uuid NOT NULL,
    username character varying(255) NOT NULL,
    name character varying(255) NOT NULL,
    email character varying(255) NOT NULL,
    admin boolean DEFAULT false NOT NULL,
    password character varying(255) NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    segment_metadata jsonb
);




CREATE TABLE public.canvases (
    id uuid NOT NULL,
    account_id uuid NOT NULL,
    name character varying(64) NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    cors_setting json
);




CREATE TABLE public.cron_records (
    id integer NOT NULL,
    tlid bigint NOT NULL,
    canvas_id uuid NOT NULL,
    ran_at timestamp without time zone DEFAULT now() NOT NULL
);




CREATE SEQUENCE public.cron_records_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;




ALTER SEQUENCE public.cron_records_id_seq OWNED BY public.cron_records.id;



CREATE TABLE public.custom_domains (
    host text NOT NULL,
    canvas text
);




CREATE TABLE public.events (
    id integer NOT NULL,
    status public.queue_status NOT NULL,
    dequeued_by integer,
    canvas_id uuid NOT NULL,
    account_id uuid NOT NULL,
    space text NOT NULL,
    name text NOT NULL,
    value text NOT NULL,
    retries integer DEFAULT 0 NOT NULL,
    delay_until timestamp without time zone DEFAULT now() NOT NULL,
    modifier text NOT NULL,
    enqueued_at timestamp with time zone,
    last_processed_at timestamp with time zone
);




CREATE SEQUENCE public.events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;




ALTER SEQUENCE public.events_id_seq OWNED BY public.events.id;



CREATE TABLE public.events_v2 (
    id uuid NOT NULL,
    canvas_id uuid NOT NULL,
    module text NOT NULL,
    name text NOT NULL,
    modifier text NOT NULL,
    locked_at timestamp with time zone,
    enqueued_at timestamp with time zone DEFAULT now() NOT NULL,
    value text NOT NULL
);




CREATE TABLE public.function_arguments (
    canvas_id uuid NOT NULL,
    tlid bigint NOT NULL,
    "timestamp" timestamp with time zone NOT NULL,
    arguments_json text NOT NULL,
    trace_id uuid NOT NULL
);




CREATE TABLE public.function_results_v3 (
    canvas_id uuid NOT NULL,
    tlid bigint NOT NULL,
    fnname text NOT NULL,
    id bigint NOT NULL,
    hash text NOT NULL,
    "timestamp" timestamp with time zone NOT NULL,
    value text NOT NULL,
    trace_id uuid NOT NULL,
    hash_version integer
);




CREATE TABLE public.op_ctrs (
    canvas_id uuid NOT NULL,
    browser_id uuid NOT NULL,
    ctr integer DEFAULT 0 NOT NULL,
    "timestamp" timestamp without time zone DEFAULT now() NOT NULL
);




CREATE TABLE public.packages_v0 (
    tlid bigint,
    user_id uuid NOT NULL,
    package text NOT NULL,
    module text NOT NULL,
    fnname text NOT NULL,
    version integer NOT NULL,
    description text NOT NULL,
    body bytea NOT NULL,
    return_type text NOT NULL,
    parameters jsonb NOT NULL,
    author_id uuid NOT NULL,
    deprecated boolean NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    body2 bytea
);




CREATE TABLE public.registered_tunnelhosts (
    user_id uuid NOT NULL,
    tunnel_host character varying(255) NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL
);




CREATE TABLE public.scheduling_rules (
    id integer NOT NULL,
    rule_type public.scheduling_rule_type NOT NULL,
    canvas_id uuid NOT NULL,
    handler_name text NOT NULL,
    event_space text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);




CREATE SEQUENCE public.scheduling_rules_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;




ALTER SEQUENCE public.scheduling_rules_id_seq OWNED BY public.scheduling_rules.id;



CREATE TABLE public.secrets (
    canvas_id uuid NOT NULL,
    secret_name character varying(255) NOT NULL,
    secret_value text NOT NULL,
    secret_version integer DEFAULT 0 NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);




CREATE TABLE public.session (
    session_key character(40),
    expire_date timestamp(2) with time zone,
    session_data text
);




CREATE TABLE public.static_asset_deploys (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    canvas_id uuid NOT NULL,
    branch character varying(255) DEFAULT 'main'::character varying NOT NULL,
    deploy_hash character varying(255) NOT NULL,
    created_at timestamp(2) with time zone DEFAULT now() NOT NULL,
    live_at timestamp(2) with time zone,
    uploaded_by_account_id uuid NOT NULL
);




CREATE TABLE public.stored_events_v2 (
    canvas_id uuid NOT NULL,
    module text NOT NULL,
    path text NOT NULL,
    modifier text NOT NULL,
    "timestamp" timestamp with time zone NOT NULL,
    value text NOT NULL,
    trace_id uuid NOT NULL
);




CREATE TABLE public.system_migrations (
    name text NOT NULL,
    execution_date timestamp with time zone NOT NULL,
    sql text NOT NULL
);




CREATE TABLE public.toplevel_oplists (
    canvas_id uuid NOT NULL,
    account_id uuid NOT NULL,
    tlid bigint NOT NULL,
    digest character(32) NOT NULL,
    tipe public.toplevel_type,
    name text,
    module text,
    modifier text,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    data bytea NOT NULL,
    rendered_oplist_cache bytea,
    deleted boolean,
    pos json,
    oplist bytea,
    oplist_cache bytea
);




CREATE TABLE public.traces_v0 (
    canvas_id uuid NOT NULL,
    root_tlid bigint NOT NULL,
    trace_id uuid NOT NULL,
    callgraph_tlids bigint[] NOT NULL
);




CREATE TABLE public.user_data (
    id uuid NOT NULL,
    account_id uuid NOT NULL,
    canvas_id uuid NOT NULL,
    table_tlid bigint NOT NULL,
    user_version integer NOT NULL,
    dark_version integer NOT NULL,
    data jsonb NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    key text NOT NULL
);




ALTER TABLE ONLY public.cron_records ALTER COLUMN id SET DEFAULT nextval('public.cron_records_id_seq'::regclass);



ALTER TABLE ONLY public.events ALTER COLUMN id SET DEFAULT nextval('public.events_id_seq'::regclass);



ALTER TABLE ONLY public.scheduling_rules ALTER COLUMN id SET DEFAULT nextval('public.scheduling_rules_id_seq'::regclass);



ALTER TABLE ONLY public.accounts
    ADD CONSTRAINT accounts_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.accounts
    ADD CONSTRAINT accounts_username_key UNIQUE (username);



ALTER TABLE ONLY public.static_asset_deploys
    ADD CONSTRAINT canvas_deploy_hash_uniq UNIQUE (canvas_id, deploy_hash);



ALTER TABLE ONLY public.canvases
    ADD CONSTRAINT canvases_name_key UNIQUE (name);



ALTER TABLE ONLY public.canvases
    ADD CONSTRAINT canvases_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.cron_records
    ADD CONSTRAINT cron_records_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.custom_domains
    ADD CONSTRAINT custom_domains_pkey PRIMARY KEY (host);



ALTER TABLE ONLY public.accounts
    ADD CONSTRAINT emails UNIQUE (email);



ALTER TABLE ONLY public.events
    ADD CONSTRAINT events_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.events_v2
    ADD CONSTRAINT events_v2_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.op_ctrs
    ADD CONSTRAINT op_ctrs_browser_id_key UNIQUE (browser_id);



ALTER TABLE ONLY public.packages_v0
    ADD CONSTRAINT packages_v0_pkey PRIMARY KEY (user_id, package, module, fnname, version);



ALTER TABLE ONLY public.registered_tunnelhosts
    ADD CONSTRAINT registered_tunnelhosts_pkey PRIMARY KEY (user_id);



ALTER TABLE ONLY public.scheduling_rules
    ADD CONSTRAINT scheduling_rules_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.secrets
    ADD CONSTRAINT secrets_pkey PRIMARY KEY (canvas_id, secret_name, secret_version);



ALTER TABLE ONLY public.static_asset_deploys
    ADD CONSTRAINT static_asset_deploys_pkey PRIMARY KEY (id);



ALTER TABLE ONLY public.system_migrations
    ADD CONSTRAINT system_migrations_pkey PRIMARY KEY (name);



ALTER TABLE ONLY public.toplevel_oplists
    ADD CONSTRAINT toplevel_oplists_pkey PRIMARY KEY (canvas_id, tlid);



ALTER TABLE ONLY public.traces_v0
    ADD CONSTRAINT traces_v0_pkey PRIMARY KEY (canvas_id, root_tlid, trace_id);



ALTER TABLE ONLY public.user_data
    ADD CONSTRAINT user_data_key_uniq UNIQUE (account_id, canvas_id, table_tlid, dark_version, user_version, key);



ALTER TABLE ONLY public.user_data
    ADD CONSTRAINT user_data_pkey PRIMARY KEY (id);



CREATE UNIQUE INDEX access_organization ON public.access USING btree (access_account, organization_account);



CREATE INDEX function_arguments_for_trace ON public.function_arguments USING btree (canvas_id, tlid, trace_id);



CREATE INDEX function_arguments_most_recent ON public.function_arguments USING btree (canvas_id, tlid, "timestamp" DESC);



CREATE INDEX idx_cron_records_tlid_canvas_id_id ON public.cron_records USING btree (tlid, canvas_id, id DESC);



CREATE INDEX idx_events_for_dequeue ON public.events USING btree (status, id) WHERE (status <> 'done'::public.queue_status);



CREATE INDEX idx_events_for_dequeue2 ON public.events USING btree (status, id) WHERE (status = 'scheduled'::public.queue_status);



CREATE INDEX idx_eventsv2_count ON public.events_v2 USING btree (canvas_id, module, name);



CREATE INDEX idx_function_results_v3_most_recent ON public.function_results_v3 USING btree (canvas_id, trace_id, tlid, "timestamp" DESC);



CREATE INDEX idx_op_ctrs_browser_id_ctr ON public.op_ctrs USING btree (browser_id, ctr DESC);



CREATE INDEX idx_op_ctrs_canvas_id ON public.op_ctrs USING btree (canvas_id);



CREATE INDEX idx_op_ctrs_timestamp ON public.op_ctrs USING btree ("timestamp");



CREATE INDEX idx_static_asset_deploys_canvas_id_branch_live_at_created_at ON public.static_asset_deploys USING btree (canvas_id, branch, live_at, created_at);



CREATE INDEX idx_static_asset_deploys_canvas_id_created_at_desc ON public.static_asset_deploys USING btree (canvas_id, created_at DESC);



CREATE INDEX idx_static_asset_deploys_canvas_live_at ON public.static_asset_deploys USING btree (canvas_id, live_at);



CREATE INDEX idx_stored_events_v2_most_recent ON public.stored_events_v2 USING btree (canvas_id, module, path, modifier, "timestamp" DESC);



CREATE INDEX idx_stored_events_v2_most_recent_with_text ON public.stored_events_v2 USING btree (canvas_id, module, path text_pattern_ops, modifier, "timestamp" DESC);



CREATE INDEX idx_toplevel_oplists_filter ON public.toplevel_oplists USING btree (module, modifier, name, deleted);



CREATE UNIQUE INDEX idx_uniq_scheduling_rules ON public.scheduling_rules USING btree (canvas_id, rule_type, handler_name, event_space);



CREATE INDEX idx_user_data_current_data_for_tlid ON public.user_data USING btree (user_version, dark_version, account_id, canvas_id, table_tlid);



CREATE INDEX idx_user_data_fetch ON public.user_data USING btree (account_id, canvas_id, table_tlid, user_version, dark_version);



CREATE INDEX idx_user_data_gin_data ON public.user_data USING gin (data jsonb_path_ops);



CREATE INDEX index_events_for_stats ON public.events USING btree (status, canvas_id);



CREATE INDEX session_key_idx ON public.session USING btree (session_key);



CREATE INDEX stored_events_v2_traceid ON public.stored_events_v2 USING btree (canvas_id, trace_id);



CREATE TRIGGER set_account_timestamp BEFORE UPDATE ON public.accounts FOR EACH ROW EXECUTE PROCEDURE public.trigger_set_timestamp();



CREATE TRIGGER set_canvas_timestamp BEFORE UPDATE ON public.canvases FOR EACH ROW EXECUTE PROCEDURE public.trigger_set_timestamp();



CREATE TRIGGER set_registered_tunnelhosts_timestamp BEFORE UPDATE ON public.registered_tunnelhosts FOR EACH ROW EXECUTE PROCEDURE public.trigger_set_timestamp();

CREATE TRIGGER set_toplevel_oplist_timestamp BEFORE UPDATE ON public.toplevel_oplists FOR EACH ROW EXECUTE PROCEDURE public.trigger_set_timestamp();

CREATE TRIGGER set_user_data_timestamp BEFORE UPDATE ON public.user_data FOR EACH ROW EXECUTE PROCEDURE public.trigger_set_timestamp();

ALTER TABLE ONLY public.canvases
    ADD CONSTRAINT canvases_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.accounts(id);

ALTER TABLE ONLY public.cron_records
    ADD CONSTRAINT cron_records_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.events
    ADD CONSTRAINT events_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.accounts(id);

ALTER TABLE ONLY public.events
    ADD CONSTRAINT events_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.function_arguments
    ADD CONSTRAINT function_arguments_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.function_results_v3
    ADD CONSTRAINT function_results_v3_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.packages_v0
    ADD CONSTRAINT packages_v0_author_id_fkey FOREIGN KEY (author_id) REFERENCES public.accounts(id);

ALTER TABLE ONLY public.packages_v0
    ADD CONSTRAINT packages_v0_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.accounts(id);

ALTER TABLE ONLY public.scheduling_rules
    ADD CONSTRAINT scheduling_rules_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.secrets
    ADD CONSTRAINT secrets_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.static_asset_deploys
    ADD CONSTRAINT static_asset_deploys_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.static_asset_deploys
    ADD CONSTRAINT static_asset_deploys_uploaded_by_account_id_fkey FOREIGN KEY (uploaded_by_account_id) REFERENCES public.accounts(id);

ALTER TABLE ONLY public.stored_events_v2
    ADD CONSTRAINT stored_events_v2_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.toplevel_oplists
    ADD CONSTRAINT toplevel_oplists_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.accounts(id);

ALTER TABLE ONLY public.toplevel_oplists
    ADD CONSTRAINT toplevel_oplists_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

ALTER TABLE ONLY public.user_data
    ADD CONSTRAINT user_data_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.accounts(id);

ALTER TABLE ONLY public.user_data
    ADD CONSTRAINT user_data_canvas_id_fkey FOREIGN KEY (canvas_id) REFERENCES public.canvases(id);

