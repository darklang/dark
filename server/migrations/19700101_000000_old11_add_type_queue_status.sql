IF NOT EXISTS
  (SELECT 1 FROM pg_type WHERE typname = 'queue_status')
THEN
  CREATE TYPE queue_status AS
  ENUM ('new', 'locked', 'done', 'error');
END IF
