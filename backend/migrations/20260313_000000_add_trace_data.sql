-- Add trace data columns to traces_v0 for storing input vars and function results
ALTER TABLE traces_v0 ADD COLUMN handler_desc TEXT NOT NULL DEFAULT '';
ALTER TABLE traces_v0 ADD COLUMN input_data TEXT NOT NULL DEFAULT '[]';
ALTER TABLE traces_v0 ADD COLUMN function_results TEXT NOT NULL DEFAULT '[]';
ALTER TABLE traces_v0 ADD COLUMN timestamp TEXT NOT NULL DEFAULT '';
