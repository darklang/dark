# Clean backend/static

**Status**: [x] Complete

## What's in backend/static?

Static assets served by the Dark backend for web interface:
- `dark-wasm-webworker.js` - Loads Dark WASM in web worker
- `editor-bootstrap.js` - Editor initialization
- `webworker-fake-env.js` - Fake DOM environment for worker
- `favicon-32x32.png` - **KEEP**
- `README.md` - Can delete

## Files to Delete

```
backend/static/
  dark-wasm-webworker.js    <- DELETE
  editor-bootstrap.js       <- DELETE
  webworker-fake-env.js     <- DELETE
  README.md                 <- DELETE
  favicon-32x32.png         <- KEEP
```

## Steps

1. [ ] Delete `backend/static/dark-wasm-webworker.js`
2. [ ] Delete `backend/static/editor-bootstrap.js`
3. [ ] Delete `backend/static/webworker-fake-env.js`
4. [ ] Delete `backend/static/README.md`
5. [ ] Verify `favicon-32x32.png` remains
6. [ ] Check if any code references these static files
7. [ ] Run `./scripts/run-backend-tests`
8. [ ] Wait for build
9. [ ] Commit: `trim: clean backend/static, keep only favicon`

## Search Commands

```bash
grep -r "dark-wasm-webworker\|editor-bootstrap\|webworker-fake-env" --include="*.fs" --include="*.js" --include="*.ts" backend/
```

## Commit Message Template

```
trim: clean backend/static, keep only favicon

- Delete dark-wasm-webworker.js
- Delete editor-bootstrap.js
- Delete webworker-fake-env.js
- Delete README.md
- Keep favicon-32x32.png

These JS files supported the WASM browser editor, now removed.
```

## Notes

- This is a quick cleanup after removing the WASM project
- The favicon might still be used by BwdServer or other services
- If nothing uses favicon either, we can remove it later
