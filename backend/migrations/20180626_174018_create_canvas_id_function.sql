CREATE OR REPLACE FUNCTION canvas_id(_new_id uuid, _account_id uuid, _name VARCHAR(40), OUT _id uuid) AS
  $func$
  BEGIN
  LOOP
    SELECT id
    FROM   canvases
    WHERE  name = _name
    INTO   _id;

    EXIT WHEN FOUND;

    INSERT INTO canvases AS c
    (id, account_id, name)
    VALUES (_new_id, _account_id, _name)
    ON     CONFLICT (name) DO NOTHING
    RETURNING c.id
    INTO   _id;

    EXIT WHEN FOUND;
  END LOOP;
  END;
$func$ LANGUAGE plpgsql
