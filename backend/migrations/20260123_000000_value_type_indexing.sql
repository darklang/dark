-- Add type indexing to package values for efficient type-based discovery
-- This enables finding all values of a specific type (e.g., all HttpHandler values)

-- Add column to store the serialized ValueType of each value
-- This is a binary blob that represents the full type (primitives, custom types, etc.)
ALTER TABLE package_values ADD COLUMN value_type BLOB;

-- Create index for efficient type-based queries (exact match on serialized type)
CREATE INDEX IF NOT EXISTS idx_package_values_type
ON package_values(value_type);
