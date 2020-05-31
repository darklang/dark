## Editing other BS libraries

We sometimes have to edit other bs libraries in tandem with our codebase, which
is a little challenging. Here are the steps to make it work:

In client/package.json
```
-    "bucklescript-tea": "darklang/bucklescript-tea#master",
+    "bucklescript-tea": "file:../../bucklescript-tea",
```

In scripts/builder:
```
+  MOUNTS="$MOUNTS --mount type=bind,src=$PWD/../bucklescript-tea,dst=/home/dark/bucklescript-tea"
```


