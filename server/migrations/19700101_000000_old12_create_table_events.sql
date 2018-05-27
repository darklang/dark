CREATE TABLE IF NOT EXISTS
events
(id SERIAL PRIMARY KEY
, status queue_status NOT NULL
, dequeued_by INT
, canvas_id UUID REFERENCES canvases(id) NOT NULL
, account_id UUID REFERENCES accounts(id) NOT NULL
, space TEXT NOT NULL
, name TEXT NOT NULL
, value TEXT NOT NULL
, retries INTEGER DEFAULT 0 NOT NULL
, flag_context TEXT DEFAULT '' NOT NULL
, delay_until TIMESTAMP NOT NULL DEFAULT NOW()
)
