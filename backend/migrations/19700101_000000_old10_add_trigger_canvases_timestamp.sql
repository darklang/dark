DROP TRIGGER IF EXISTS set_canvas_timestamp
ON canvases;

CREATE TRIGGER set_canvas_timestamp
BEFORE UPDATE ON canvases
FOR EACH ROW
EXECUTE PROCEDURE trigger_set_timestamp()
