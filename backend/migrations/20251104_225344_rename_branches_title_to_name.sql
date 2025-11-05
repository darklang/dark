-- Rename branches.title column to branches.name for consistency with the codebase
ALTER TABLE branches RENAME COLUMN title TO name;
