/* This has a primary key so that we can put the key into the body and
 * associate it back to the function. */
CREATE TABLE IF NOT EXISTS packages_v0
( tlid BIGINT
  /* owner/namespace part of the string, eg dark */
, user_id UUID REFERENCES accounts (id) NOT NULL 
, package TEXT NOT NULL /* eg stdlib */
, module TEXT NOT NULL /* eg Twitter */
, fnname TEXT NOT NULL /* eg sendText */
, version INTEGER NOT NULL /* eg 0 */
, description TEXT NOT NULL /* docstring */
, body BYTEA NOT NULL
, return_type TEXT NOT NULL
, parameters jsonb NOT NULL
, author_id UUID REFERENCES accounts(id) NOT NULL /* who uploaded this */
, deprecated BOOL NOT NULL
, updated_at TIMESTAMP NOT NULL DEFAULT NOW()
, created_at TIMESTAMP NOT NULL DEFAULT NOW()
, PRIMARY KEY (user_id, package, module, fnname, version)
)


