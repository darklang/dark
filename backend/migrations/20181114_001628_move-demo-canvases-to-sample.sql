-- Note: the WHERE clause means we won't touch, say, "someuser-demo-thing"
-- (Because replace is a substr replace, not regexp replace)
UPDATE canvases
SET name = replace(name, 'demo-', 'sample-')
WHERE name LIKE 'demo-%'
