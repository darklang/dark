CREATE TYPE scheduling_rule_type AS ENUM ('pause', 'block');

CREATE TABLE IF NOT EXISTS scheduling_rules (
    id SERIAL PRIMARY KEY,
    rule_type scheduling_rule_type NOT NULL,
    canvas_id UUID REFERENCES canvases(id) NOT NULL,
    handler_name TEXT NOT NULL,
    event_space TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX idx_uniq_scheduling_rules
ON scheduling_rules (canvas_id, rule_type, handler_name, event_space);

DROP INDEX idx_dequeue;
DROP INDEX idx_cleanup
