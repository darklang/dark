# Analyze Compile Scripts for Dev Cycle Optimization

**Status**: [x] Complete

## Objective

Research the build system to identify opportunities to make dev cycles faster.

## Key Files to Analyze

### Primary Build Scripts
- `scripts/build/compile` - Main Python build orchestrator
- `scripts/build/_build-server` - Background build server
- `scripts/build/_dotnet-wrapper` - .NET wrapper with server kill logic

### Supporting Scripts
- `scripts/build/build-parser` - Tree-sitter parser build
- `scripts/build/compile-project` - Project compilation
- `scripts/build/reload-packages` - Package reload

## Questions to Answer

1. **Build triggers**: What triggers a rebuild? Is it too aggressive?
2. **Parallelization**: Are independent builds running in parallel?
3. **Caching**: Is build caching optimal? Are we rebuilding unnecessarily?
4. **Hot reload**: Is there opportunity for faster hot-reload?
5. **Dependencies**: Are we tracking dependencies correctly?
6. **Tests**: Are tests running when they shouldn't?
7. **Package reload**: Is the 10s package reload time reducible?

## Metrics to Collect

- Time for F# file change to be reflected
- Time for .dark file change to be reflected
- Time for full rebuild
- Time for incremental rebuild
- CPU/memory usage during builds

## Analysis Output

Create a report file: `docs/dev-cycle-analysis.md` with:
- Current timings
- Identified bottlenecks
- Recommended improvements
- Questions for discussion

## Steps

1. [ ] Read and understand `scripts/build/compile`
2. [ ] Read and understand `scripts/build/_build-server`
3. [ ] Instrument/time various build phases
4. [ ] Identify any obvious inefficiencies
5. [ ] Document findings in `docs/dev-cycle-analysis.md`
6. [ ] List actionable improvements
7. [ ] Commit: `analysis: document dev cycle optimization opportunities`

## Commit Message Template

```
analysis: document dev cycle optimization opportunities

- Add docs/dev-cycle-analysis.md with build system analysis
- Document current timings and bottlenecks
- Propose improvements for faster dev cycles

This is research/documentation only, no code changes.
```

## Investigation Areas

### compile script
- File watching mechanism
- Build phase dependencies
- Error handling and recovery

### .NET build
- Is `dotnet build` vs `dotnet publish` appropriate?
- Are we using incremental builds effectively?
- Is the solution file structure optimal?

### Package reload
- What triggers a reload?
- Can we do partial reloads?
- Is SQLite the bottleneck?

## Notes

- This task is research only - don't make changes yet
- Collect concrete data/timings
- Discuss findings before implementing changes
- Some improvements may be quick wins, others may be significant work
