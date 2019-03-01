CREATE INDEX idx_static_asset_deploys_canvas_id_branch_live_at_created_at
ON idx_static_asset_deploys (canvas_id, branch, live_at, created_at);
