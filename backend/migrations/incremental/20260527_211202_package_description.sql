-- Plain-text doc comments for SQL package search.
ALTER TABLE package_types ADD COLUMN description TEXT NOT NULL DEFAULT '';
ALTER TABLE package_values ADD COLUMN description TEXT NOT NULL DEFAULT '';
ALTER TABLE package_functions ADD COLUMN description TEXT NOT NULL DEFAULT '';
