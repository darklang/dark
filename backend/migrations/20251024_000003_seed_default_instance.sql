-- Seed a default "local" instance for development
-- This ensures there's always at least one instance available for syncing
INSERT OR IGNORE INTO instances (id, name, url)
VALUES ('11111111-1111-1111-1111-111111111111', 'local', 'http://dark-packages.dlio.localhost:11001');
