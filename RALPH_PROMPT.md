# Ralph Wiggum Loop: Faster Package Reloading

You are in a Ralph Wiggum loop. Your goal is to make package reloading MUCH faster.

## The Goal
When a SINGLE .dark file changes, the package repository should be updated in **under 1 second**.
Currently it takes ~15 seconds because it reloads EVERYTHING.

## CRITICAL INSIGHT: Think From The Ground Up

The current approach is fundamentally wrong:
- It parses ALL 276 files twice (even when only 1 changed)
- It purges ALL data from the database
- It re-inserts ALL 2600+ items

**The right approach is INCREMENTAL:**
- Track which files have been parsed and their content hashes
- When a file changes, ONLY re-parse that one file
- ONLY update the ops from that file in the database
- Don't touch anything else

## What You Must Do This Iteration

1. **Check git log** - See what's been done already
2. **Check FASTER_RELOAD_PLAN.md** - Understand the strategy
3. **Think from the ground up**:
   - What's the minimum work needed when ONE file changes?
   - How can we track which ops came from which file?
   - How can we detect only the changed files?
4. **Implement the next piece** of the incremental system
5. **Test with**: `time ./scripts/build/reload-packages`
6. **Commit** if it works

## Key Architecture Changes Needed

### 1. File Hash Tracking
- Store (file_path, content_hash) somewhere (table or file)
- On reload: check which files have changed content hashes
- Only process changed files

### 2. File-to-Ops Mapping
- Track which PackageOps came from which source file
- Store source_file in package_ops table or separate table
- When a file changes: delete old ops from that file, insert new ones

### 3. Incremental Update Flow
```
1. List all .dark files and their current hashes
2. Compare to stored hashes
3. For changed files only:
   a. Delete their old ops from package_ops
   b. Parse the file
   c. Insert new ops
   d. Apply new ops to projections
4. Update stored hashes
```

## Key Files

| File | What to change |
|------|---------------|
| `backend/src/LocalExec/LoadPackagesFromDisk.fs` | Add incremental logic |
| `backend/src/LibPackageManager/Purge.fs` | Add selective purge by source file |
| `backend/src/LibPackageManager/Inserts.fs` | Track source file in ops |
| `backend/migrations/YYYYMMDD_*.sql` | Add file_hashes table, add source_file column |

## Rules

- Think about the MINIMAL work needed for a single file change
- The goal is <1s for single file changes, not for full reload
- Test incremental behavior, not just full reload
- Commit working changes

## DO NOT

- Try to optimize the full-reload path (that's the wrong approach)
- Make changes without testing
- Forget that we need INCREMENTAL, not just FASTER-FULL-RELOAD

## ALL POSSIBLE APPROACHES (from FASTER_RELOAD_PLAN.md)

1. **Incremental file reload** - Track hashes, only reload changed files
2. **In-memory only** - Skip DB for dev, memory-first
3. **File watcher daemon** - Persistent process with inotify
4. **Lazy loading** - Load on demand, invalidate cache on change
5. **Skip two-pass parsing** - Single pass with deferred resolution
6. **Pre-compiled binaries** - Store binary cache alongside .dark
7. **Content-addressed skip** - Don't purge, use INSERT OR IGNORE!
8. **Hybrid (RECOMMENDED)** - Hash tracking + content-addressed skip
9. **Memory-first + background sync** - Fast memory, async DB

**Key Insight**: Option 7/8 leverage EXISTING infrastructure!
- Ops are already content-addressed (SHA256 hash as ID)
- INSERT OR IGNORE already skips duplicates
- We already track which ops were actually inserted
- If we STOP PURGING, unchanged ops are automatically skipped!

## Quick Experiment To Try First

Before implementing anything complex, try this:
1. Comment out the `Purge.purge()` call in LocalExec.fs
2. Run reload-packages twice
3. Second run should be MUCH faster (nothing to insert!)

This tests whether the content-addressed skip actually works.

## Measuring Success

```bash
# Full reload (baseline, expected to still be slow)
time ./scripts/build/reload-packages

# Second run without purge (should be fast if skip works!)
time ./scripts/build/reload-packages

# Single file change (THIS is what we're optimizing)
touch packages/darklang/stdlib/bool.dark
time ./scripts/build/reload-packages
# Should be <1s after changes
```
