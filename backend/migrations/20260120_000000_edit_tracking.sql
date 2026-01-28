-- Edit tracking: Track when package items are edited (content changes with new UUID)
-- This enables tracking edit history and notifying dependent owners of breaking changes.

-- Edit history: records when an item is edited (replaced by a new version)
CREATE TABLE IF NOT EXISTS edit_history (
  id TEXT PRIMARY KEY,
  old_item_id TEXT NOT NULL,          -- the previous version's UUID
  new_item_id TEXT NOT NULL,          -- the new version's UUID
  is_breaking INTEGER NOT NULL,       -- 1 if signature changed, 0 if compatible
  branch_id TEXT,                     -- NULL for main branch
  created_at TEXT DEFAULT (datetime('now')),
  created_by TEXT                     -- account ID that made the edit
);

-- Index for finding edits by old item (what replaced this?)
CREATE INDEX IF NOT EXISTS idx_edit_history_old_item ON edit_history(old_item_id);

-- Index for finding edits by new item (what did this replace?)
CREATE INDEX IF NOT EXISTS idx_edit_history_new_item ON edit_history(new_item_id);

-- Index for finding edits by branch
CREATE INDEX IF NOT EXISTS idx_edit_history_branch ON edit_history(branch_id);


-- Breaking change todos: tracks when dependents need to update due to breaking changes
-- Created when a breaking edit is detected; resolved when the dependent is updated
--
-- Status values:
--   'pending'   - awaiting action
--   'resolved'  - fixed by the dependent owner
--   'dismissed' - dismissed without fixing
--   'applied'   - compatible update was automatically applied (can be reverted)
--
-- Change type values:
--   'breaking'   - signature changed, requires manual fix
--   'compatible' - implementation only, can be auto-applied
CREATE TABLE IF NOT EXISTS breaking_change_todos (
  id TEXT PRIMARY KEY,
  edit_id TEXT NOT NULL REFERENCES edit_history(id) ON DELETE CASCADE,
  dependent_item_id TEXT NOT NULL,    -- the item that depends on the edited item
  owner_id TEXT NOT NULL,             -- account that owns the dependent item
  status TEXT DEFAULT 'pending',
  change_type TEXT NOT NULL DEFAULT 'breaking',
  created_at TEXT DEFAULT (datetime('now')),
  resolved_at TEXT,                   -- when status changed from pending
  resolved_by TEXT,                   -- account that resolved/dismissed
  applied_at TEXT,                    -- when an update was applied
  previous_dependent_id TEXT          -- UUID of dependent before update (for revert)
);

-- Index for finding todos by owner (for "show my todos" queries)
CREATE INDEX IF NOT EXISTS idx_breaking_todos_owner ON breaking_change_todos(owner_id);

-- Index for finding todos by status (for filtering pending vs resolved)
CREATE INDEX IF NOT EXISTS idx_breaking_todos_status ON breaking_change_todos(status);

-- Index for finding todos by dependent item (for checking if an item has pending updates)
CREATE INDEX IF NOT EXISTS idx_breaking_todos_dependent ON breaking_change_todos(dependent_item_id);

-- Index for finding todos by edit (for looking up all affected dependents of an edit)
CREATE INDEX IF NOT EXISTS idx_breaking_todos_edit ON breaking_change_todos(edit_id);

-- Index for efficient filtering by change type
CREATE INDEX IF NOT EXISTS idx_breaking_todos_change_type ON breaking_change_todos(change_type);
