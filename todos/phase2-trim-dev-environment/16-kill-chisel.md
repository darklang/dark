# Kill Chisel

**Status**: [ ] Not started

## What is Chisel?

Chisel is an HTTP tunneling tool used by ProdExec to establish SSH connections to production through HTTP. Since ProdExec is deleted, Chisel is no longer needed.

## Dockerfile Changes

Remove Chisel installation:
```dockerfile
############################
# Chisel
############################
RUN /home/dark/install-gz-file \
  --arm64-sha256=05f5eabab4a5f65f2bb08d967d6af41247465af213f1c874ad0e059c0a3ebedc \
  --amd64-sha256=704a31cd89911a0f7d1741ee9ca32ca0f5496b06370bf398dfc5b7d3a31ef563 \
  --url=https://github.com/jpillora/chisel/releases/download/v1.9.1/chisel_1.9.1_linux_${TARGETARCH}.gz \
  --target=/usr/bin/chisel
```

## Search for References

```bash
grep -r "chisel" --include="*.sh" --include="Dockerfile" --include="*.md" .
```

## Steps

1. [ ] Remove Chisel installation section from `Dockerfile`
2. [ ] Search for any remaining chisel references
3. [ ] Run `./scripts/run-backend-tests`
4. [ ] Commit: `trim: remove Chisel`

## Commit Message Template

```
trim: remove Chisel

- Remove Chisel installation from Dockerfile

Chisel was used for HTTP-to-SSH tunneling for ProdExec.
ProdExec already removed, so Chisel no longer needed.
```

## Notes

- Quick cleanup task
- Part of ProdExec-related cleanup
