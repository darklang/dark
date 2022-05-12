CREATE TABLE IF NOT EXISTS
events_v2
( id UUID PRIMARY KEY
, canvas_id UUID NOT NULL
, module text NOT NULL
, name text NOT NULL
, modifier text NOT NULL
, locked_at TIMESTAMP WITH TIME ZONE -- nullable
, enqueued_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
, value text NOT NULL)