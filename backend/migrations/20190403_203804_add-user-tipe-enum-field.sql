ALTER TYPE toplevel_type RENAME TO toplevel_type_old_;

CREATE TYPE toplevel_type AS ENUM ('handler', 'db', 'user_function', 'user_tipe');

ALTER TABLE toplevel_oplists ALTER COLUMN tipe TYPE toplevel_type USING tipe::text::toplevel_type;

DROP TYPE toplevel_type_old_
