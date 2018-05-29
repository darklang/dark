CREATE TABLE IF NOT EXISTS
function_results
( canvas_id UUID REFERENCES canvases(id) NOT NULL
, tlid INT NOT NULL
, fnname TEXT NOT NULL
, id INT NOT NULL
, hash TEXT NOT NULL
, timestamp TIMESTAMPTZ NOT NULL
, value TEXT NOT NULL
)
