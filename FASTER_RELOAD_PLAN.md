# Faster Package Reloading Plan

## Current State
- **276 .dark files** parsed
- **594 types, 154 values, 1884 functions** loaded
- **Current time: ~15 seconds**
- **Target: <1 second** (15x improvement)

---

## ALL POSSIBLE APPROACHES (Think From Ground Up)

### Option 1: Incremental File-Based Reload
**Concept**: Track file hashes, only reload changed files
- Store (file_path, content_hash)
- On reload: compare hashes, only parse changed files
- Delete old ops from changed files, insert new ones
- **Estimated time**: 0.5-2s per changed file
- **Complexity**: Medium
- **Pros**: Straightforward extension of current system
- **Cons**: Still has overhead from DB operations, two-pass parsing

### Option 2: In-Memory Only (Dev Mode)
**Concept**: Skip DB entirely for development
- Parse once at startup, keep in memory
- On file change, update in-memory structure directly
- DB only for production/persistence
- **Estimated time**: <100ms per file
- **Complexity**: Medium-High
- **Pros**: Very fast, no DB overhead
- **Cons**: Requires dual code paths, no persistence during dev

### Option 3: Long-Running File Watcher Daemon
**Concept**: Replace reload-packages script with persistent process
- Use inotify/fswatch to detect changes instantly
- Parse changed file immediately on change detection
- Update in-memory PM directly
- No startup overhead per change
- **Estimated time**: <50ms per file
- **Complexity**: High
- **Pros**: Fastest possible, no polling
- **Cons**: New architecture, process management complexity

### Option 4: Lazy Loading with Cache Invalidation
**Concept**: Don't load packages upfront, load on demand
- On first access to a package, parse and cache it
- When file changes, just invalidate that cache entry
- Next access triggers re-parse
- **Estimated time**: <10ms for invalidation
- **Complexity**: Medium-High
- **Pros**: Only loads what's needed, instant invalidation
- **Cons**: First access is slow, complex cache management

### Option 5: Skip Two-Pass Parsing
**Concept**: Use single pass with deferred name resolution
- Parse all files once with placeholder references
- Resolve names lazily on first use
- **Estimated time**: ~7s (half of current)
- **Complexity**: Medium
- **Pros**: Halves parsing time
- **Cons**: Only 2x improvement, not enough

### Option 6: Pre-Compiled Binary Packages
**Concept**: Compile packages to binary format once
- Store serialized binary alongside .dark files
- On reload, just deserialize binary
- Only recompile when source changes (via hash)
- **Estimated time**: <100ms for loading binaries
- **Complexity**: Medium
- **Pros**: Very fast reload, still file-based
- **Cons**: Binary files need management, cache invalidation

### Option 7: Content-Addressed Skip (Use Existing Infrastructure!)
**Concept**: Leverage existing INSERT OR IGNORE
- DON'T purge the database first
- Parse files (only changed ones if we track hashes)
- Use INSERT OR IGNORE - unchanged ops are skipped
- Only apply ops that were actually inserted
- **Estimated time**: Could be <1s if most files unchanged
- **Complexity**: LOW - infrastructure already exists!
- **Pros**: Minimal changes, uses existing content addressing
- **Cons**: Still need to parse to generate hashes

### Option 8: Hybrid - Hash Check + Content-Addressed Skip
**Concept**: Combine file hash tracking with content-addressed ops
1. Track file content hashes
2. Skip parsing for unchanged files entirely
3. For changed files: parse, INSERT OR IGNORE
4. Only apply newly inserted ops
- **Estimated time**: <500ms for single file change
- **Complexity**: Low-Medium
- **Pros**: Best of both worlds, minimal changes
- **Cons**: Need to implement hash tracking

### Option 9: Remove DB From Hot Path
**Concept**: Memory-first with background DB sync
- All package data lives in memory
- File changes update memory instantly
- DB writes happen in background/async
- **Estimated time**: <10ms
- **Complexity**: High
- **Pros**: Blazing fast
- **Cons**: Complex state management, consistency concerns

---

## RECOMMENDED APPROACH: Option 8 (Hybrid)

This combines the best ideas with minimal changes:

1. **Add file hash tracking** - simple table or file
2. **Remove the PURGE step** - don't delete everything
3. **Skip unchanged files** - use hash comparison
4. **Parse only changed files** - major speedup
5. **Rely on INSERT OR IGNORE** - already handles duplicates
6. **Only apply new ops** - already tracking this!

### Why This Works
- Most of the infrastructure already exists
- INSERT OR IGNORE handles duplicate ops
- We already track which ops were inserted vs skipped
- We just need to:
  1. Stop purging
  2. Track file hashes
  3. Skip unchanged files

## Root Causes of Slowness

### 1. Two-Phase Parsing (Major - ~6s)
- `LoadPackagesFromDisk.fs:31-48`: First pass with `OnMissing.Allow`
- `LoadPackagesFromDisk.fs:52-65`: Second pass with `OnMissing.ThrowError`
- Parses ALL 276 files TWICE
- Sequential, not parallelized (TODO exists at line 33)

### 2. Full Purge + Rebuild (Major - ~3s)
- `Purge.fs:17-40`: Deletes ALL data from 5 tables
- `Inserts.fs:43-101`: Re-inserts everything
- No incremental updates - always full reload

### 3. Triple Serialization per Item (Major - ~4s)
- `PackageOpPlayback.fs:24`: Serialize PT to binary
- `PackageOpPlayback.fs:25`: Convert PT → RT
- `PackageOpPlayback.fs:26`: Serialize RT to binary
- Happens for ALL 594+154+1884 = 2632 items

### 4. Sequential Processing (Moderate - ~2s)
- File reading is sequential
- Parsing is sequential (marked TODO)
- Op application is sequential

## Optimization Strategy

### Phase 1: Incremental Reload (Target: 10x speedup)
**Key insight**: When a single .dark file changes, don't reload everything.

1. **Track file hashes** - Store SHA256 of each .dark file content
2. **Detect changed files** - On reload, only re-parse changed files
3. **Delta updates** - Only purge/insert ops for changed packages
4. **Skip unchanged** - Leave unchanged packages in DB as-is

Implementation:
- Add `package_file_hashes` table: (file_path, content_hash, last_modified)
- On reload: compare hashes, collect changed files only
- Parse only changed files
- Delete only ops from changed files, insert new ops

### Phase 2: Parallelize Parsing (Target: 2x speedup)
1. **Parallel file parsing** - Use `Ply.List.mapInParallel` instead of `mapSequentially`
2. **Two-phase parallel** - Both passes can be parallelized
3. **Batched DB operations** - Bulk insert instead of one-by-one

### Phase 3: Lazy RT Conversion (Target: 1.5x speedup)
1. **Don't pre-convert PT→RT** - Convert on first access instead
2. **Cache RT on demand** - Store RT blob only when needed
3. **Skip RT serialization** - Until actually used for execution

### Phase 4: Single-Pass Parsing (Target: 1.5x speedup)
1. **Lazy name resolution** - Resolve names on first access
2. **Skip ID stabilization** - Use content-addressed IDs directly
3. **Single parse per file** - Eliminate the two-phase requirement

## Implementation Order

1. **Incremental reload** - Biggest bang for buck, most files don't change
2. **Parallelize** - Easy win, TODO already exists
3. **Lazy RT** - Moderate effort, good payoff
4. **Single-pass** - Most complex, may not be needed if above suffice

## Success Metrics

After each change, measure with:
```bash
time ./scripts/build/reload-packages
```

| Phase | Target Time | Speedup |
|-------|-------------|---------|
| Baseline | 15s | 1x |
| Phase 1 (incremental) | 1.5s | 10x |
| Phase 2 (parallel) | 0.75s | 20x |
| Phase 3 (lazy RT) | 0.5s | 30x |
| Phase 4 (single-pass) | 0.3s | 50x |

## Files to Modify

### Core Changes
- `backend/src/LocalExec/LoadPackagesFromDisk.fs` - Incremental loading logic
- `backend/src/LibPackageManager/Purge.fs` - Selective purge
- `backend/src/LibPackageManager/Inserts.fs` - Delta inserts
- `backend/src/LibPackageManager/PackageOpPlayback.fs` - Lazy RT conversion

### New Files
- `backend/src/LibPackageManager/FileHashes.fs` - Track file content hashes
- `backend/migrations/YYYYMMDD_file_hashes.sql` - New table for tracking

### Supporting Changes
- `backend/src/LocalExec/LocalExec.fs` - Pass changed files info
- `scripts/build/reload-packages` - Optional: accept file list argument

## Testing Strategy

1. **Baseline test**: `time ./scripts/build/reload-packages`
2. **Single file change**: Modify one .dark file, time reload
3. **Multi-file change**: Modify 5 files, time reload
4. **Full reload**: Delete hash cache, time full reload
5. **Correctness**: Run existing tests to verify nothing broke
