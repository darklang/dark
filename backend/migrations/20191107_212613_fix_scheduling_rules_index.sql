DROP INDEX idx_uniq_scheduling_rules;

CREATE UNIQUE INDEX idx_uniq_scheduling_rules
ON scheduling_rules (canvas_id, rule_type, handler_name, event_space)
