-- ideally we could make this NOT NULL and create it at canvas creation time?
ALTER TABLE canvases
ADD COLUMN gcloud_bucket_name VARCHAR(63) DEFAULT NULL;

CREATE UNIQUE INDEX idx_canvases_gcloud_bucket_name ON canvases (gcloud_bucket_name);

CREATE TABLE IF NOT EXISTS static_asset_deploys
(
  id uuid PRIMARY KEY DEFAULT gen_random_uuid()
  , canvas_id UUID REFERENCES canvases(id) NOT NULL
  , branch VARCHAR(255) NOT NULL default 'main'
  , deploy_hash VARCHAR(255) NOT NULL -- TODO length
  , created_at TIMESTAMP (2) WITH TIME ZONE NOT NULL DEFAULT NOW()
  , live_at TIMESTAMP (2) WITH TIME ZONE
  , uploaded_by_account_id UUID REFERENCES accounts(id) NOT NULL
);

-- unique index on canvas_id+deploy_hash
CREATE UNIQUE INDEX idx_static_asset_deploys_canvas_deploy_hash ON static_asset_deploys (canvas_id, deploy_hash);
ALTER TABLE static_asset_deploys ADD constraint canvas_deploy_hash_uniq UNIQUE USING INDEX idx_static_asset_deploys_canvas_deploy_hash;

CREATE INDEX idx_static_asset_deploys_canvas_live_at ON static_asset_deploys (canvas_id, live_at)
