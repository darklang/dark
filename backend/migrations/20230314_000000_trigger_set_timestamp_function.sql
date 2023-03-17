CREATE OR REPLACE FUNCTION trigger_set_timestamp()
RETURNS TRIGGER AS $t$
  BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
  END;
$t$ LANGUAGE plpgsql