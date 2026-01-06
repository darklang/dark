# Kill Java

**Status**: [x] Complete

## What's Being Removed?

Java (OpenJDK 11) was only needed for the Google Cloud PubSub emulator. Since we removed PubSub, Java is no longer needed.

## Dockerfile Changes

Remove from apt install:
```dockerfile
openjdk-11-jdk \
```

Remove JAVA_HOME environment variable:
```dockerfile
############################
# Java version management
############################
# Set Java 11 as default for gcloud tools (including PubSub emulator)
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-${TARGETARCH}
```

## Search Commands

```bash
grep -r "java\|JAVA_HOME\|openjdk" --include="*.sh" --include="Dockerfile" .
```

## Steps

1. [ ] Edit `Dockerfile`:
   - Remove `openjdk-11-jdk` from apt install
   - Remove JAVA_HOME section
2. [ ] Search for any Java references in scripts
3. [ ] Run `./scripts/run-backend-tests`
4. [ ] Commit: `trim: remove Java/OpenJDK`

## Commit Message Template

```
trim: remove Java/OpenJDK

- Remove openjdk-11-jdk from Dockerfile apt install
- Remove JAVA_HOME environment variable

Java was only needed for PubSub emulator, now removed.
```

## Notes

- Quick change after PubSub removal
- Will reduce container size
