# Darklang Package Synchronization Protocol Specification

**Document Version**: 1.0  
**Date**: August 10, 2025  
**Status**: Board Meeting Specification  
**Authors**: Darklang Development Team  

## Executive Summary

This specification defines the comprehensive protocol for synchronizing package types, functions, constants, and values across the Darklang ecosystem. The protocol enables real-time distribution of packages from Dark Matter (the package repository) to local development environments, CLI tools, cloud runtimes, and distributed Dark applications.

## 1. Protocol Architecture Overview

### 1.1 System Components

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Dark Matter   │    │  Local Package  │    │    Runtime      │
│   (Hub/Server)  │◄──►│   Manager       │◄──►│  Environments   │
│                 │    │   (CLI/Dev)     │    │ (CLI/Cloud/Web) │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Package       │    │   Sync Client   │    │  Runtime Types  │
│   Repository    │    │   (HTTP/WS)     │    │   (RT Cache)    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### 1.2 Core Protocol Principles

- **Content-Addressable Storage**: All packages referenced by cryptographic hash
- **Incremental Sync**: Only transfer changed packages since last sync
- **Multi-Runtime Support**: PT (Program Types) and RT (Runtime Types) variants
- **Lazy Loading**: Load packages on-demand with intelligent caching
- **Version Consistency**: Ensure atomic updates across dependent packages

## 2. Package Data Model

### 2.1 Package Types Hierarchy

```fsharp
// Core package entity types
type PackageEntity =
  | PackageType of PackageType.PackageType
  | PackageFunction of PackageFn.PackageFn  
  | PackageConstant of PackageConstant.PackageConstant
  | PackageModule of PackageModule

// Universal package identifier
type PackageID = {
  hash: String                    // SHA-256 content hash
  name: PackageName              // Human-readable name
  version: Option<SemanticVersion> // Optional semantic version
}

// Fully-qualified package reference
type FQPackageRef =
  | TypeRef of FQTypeName.FQTypeName
  | FnRef of FQFnName.FQFnName
  | ConstRef of FQConstantName.FQConstantName
```

### 2.2 Synchronization Metadata

```fsharp
type SyncMetadata = {
  lastSyncTimestamp: DateTime
  serverVersion: String
  clientVersion: String
  syncMode: SyncMode
  compressedSize: Int64
  uncompressedSize: Int64
}

type SyncMode =
  | FullSync           // Complete package database download
  | IncrementalSync    // Only changed packages since last sync
  | OnDemandSync       // Lazy loading as needed
```

## 3. Package Discovery & Search Protocol

### 3.1 Search Query Protocol

```http
POST /packages/search HTTP/1.1
Content-Type: application/json
Authorization: Bearer {token}

{
  "query": {
    "currentModule": ["Darklang", "Stdlib"],
    "text": "List",
    "searchDepth": "OnlyDirectDescendants",
    "entityTypes": ["Type", "Fn", "Constant"],
    "includeRT": true,
    "includePT": true
  },
  "pagination": {
    "offset": 0,
    "limit": 50
  },
  "cacheControl": {
    "ifModifiedSince": "2025-08-10T10:30:00Z",
    "etag": "abc123"
  }
}
```

### 3.2 Search Response Protocol

```http
HTTP/1.1 200 OK
Content-Type: application/json
ETag: "def456"
Last-Modified: Mon, 10 Aug 2025 10:45:00 GMT

{
  "results": {
    "submodules": [
      ["List"], 
      ["String", "List"]
    ],
    "types": [
      {
        "id": "550e8400-e29b-41d4-a716-446655440000",
        "name": {
          "owner": "Darklang",
          "modules": ["Stdlib"],
          "name": "List"
        },
        "definition": "type List<'T> = ...",
        "hash": "sha256:abc123...",
        "dependencies": ["Option", "Result"]
      }
    ],
    "functions": [...],
    "constants": [...]
  },
  "metadata": {
    "totalCount": 156,
    "hasMore": true,
    "searchDuration": "45ms"
  }
}
```

## 4. Package Retrieval Protocol

### 4.1 Individual Package Fetch

```http
GET /packages/types/550e8400-e29b-41d4-a716-446655440000 HTTP/1.1
Accept: application/octet-stream, application/json
Authorization: Bearer {token}
If-None-Match: "abc123"

# Response contains serialized package data
HTTP/1.1 200 OK
Content-Type: application/octet-stream
ETag: "abc123"
Content-Encoding: gzip

[Binary serialized PackageType data]
```

### 4.2 Bulk Package Sync

```http
POST /packages/sync HTTP/1.1
Content-Type: application/json
Authorization: Bearer {token}

{
  "syncRequest": {
    "clientState": {
      "lastSyncTimestamp": "2025-08-10T10:00:00Z",
      "knownHashes": [
        "sha256:abc123...",
        "sha256:def456..."
      ]
    },
    "requestedTypes": ["PT", "RT"],
    "compressionPreference": "gzip",
    "maxBatchSize": 100
  }
}
```

## 5. Binary Serialization Protocol

### 5.1 Custom Binary Format

The protocol uses a custom binary serialization format optimized for Darklang's type system:

```
Package Binary Format:
┌─────────────────┬─────────────────┬─────────────────┬─────────────┐
│   Header        │   Metadata      │   Dependencies  │   Payload   │
│   (16 bytes)    │   (Variable)    │   (Variable)    │ (Variable)  │
└─────────────────┴─────────────────┴─────────────────┴─────────────┘

Header Structure:
- Magic Number: "DKLG" (4 bytes)
- Version: UInt32 (4 bytes) 
- Payload Type: UInt32 (4 bytes)
- Checksum: UInt32 (4 bytes)

Metadata Structure:
- Package ID: UUID (16 bytes)
- Content Hash: SHA-256 (32 bytes)
- Timestamp: Int64 (8 bytes)
- Flags: UInt32 (4 bytes)
```

### 5.2 Type-Specific Serialization

```fsharp
// Serialization interface for all package entities
type IPackageSerializable<'T> =
  abstract member serialize: 'T -> byte[]
  abstract member deserialize: byte[] -> Result<'T, SerializationError>

// RT-specific serialization for runtime performance
module RTSerialization =
  let serializePackageFn (fn: RT.PackageFn) : byte[] =
    // Optimized binary format for runtime execution
    
  let serializePackageType (typ: RT.PackageType) : byte[] =
    // Optimized binary format for runtime type checking

// PT-specific serialization for development tools  
module PTSerialization =
  let serializePackageFn (fn: PT.PackageFn.PackageFn) : byte[] =
    // Rich format with source information and metadata
```

## 6. Dependency Resolution & Versioning

### 6.1 Content-Addressable Dependencies

```fsharp
type PackageDependency = {
  packageRef: FQPackageRef
  contentHash: String           // SHA-256 of dependent package content
  semanticConstraint: Option<VersionConstraint>  // Optional semver constraint
  isRequired: Bool             // Required vs optional dependency
}

type VersionConstraint =
  | Exact of SemanticVersion
  | Compatible of SemanticVersion  // ^1.2.3
  | Range of min: SemanticVersion * max: SemanticVersion
  | Latest                        // Always use latest version
```

### 6.2 Dependency Resolution Algorithm

```fsharp
module DependencyResolution =
  
  type ResolutionContext = {
    knownPackages: Dict<PackageID, PackageEntity>
    versionPreferences: Dict<PackageName, VersionConstraint>
    conflictResolution: ConflictResolution
  }
  
  type ConflictResolution =
    | FailOnConflict           // Strict mode - fail if versions conflict
    | PreferLatest            // Always choose latest version
    | PreferSpecified         // Use explicitly specified versions
  
  let resolvePackageDependencies 
    (rootPackage: PackageID) 
    (context: ResolutionContext) : Result<ResolvedDependencyGraph, ResolutionError> =
    // Recursive dependency resolution with cycle detection
    // Returns topologically sorted dependency list
```

## 7. Caching & Performance Protocol

### 7.1 Multi-Level Caching Strategy

```
Caching Hierarchy:
┌─────────────────┐   L1: In-Memory Cache (Hot packages)
│   Memory Cache  │   - Recently used packages
│   (In-Process)  │   - Deserialized objects ready for use
└─────────────────┘   
         │
┌─────────────────┐   L2: Local Disk Cache (SQLite)
│  SQLite Cache   │   - Serialized packages with metadata
│   (Persistent)  │   - Indexed by hash, name, and timestamp
└─────────────────┘   
         │
┌─────────────────┐   L3: Remote Package Server
│   Dark Matter   │   - Authoritative package source
│   (Network)     │   - Full package history and metadata
└─────────────────┘   
```

### 7.2 Cache Invalidation Protocol

```http
# Server pushes cache invalidation via WebSocket
{
  "type": "cacheInvalidation",
  "payload": {
    "invalidatedPackages": [
      {
        "packageId": "550e8400-e29b-41d4-a716-446655440000",
        "reason": "PackageUpdated",
        "newHash": "sha256:newHash123"
      }
    ],
    "timestamp": "2025-08-10T11:00:00Z"
  }
}

# Client acknowledges invalidation
{
  "type": "cacheInvalidationAck",
  "payload": {
    "processedPackages": ["550e8400-e29b-41d4-a716-446655440000"],
    "timestamp": "2025-08-10T11:00:05Z"
  }
}
```

## 8. Real-Time Sync Protocol (WebSocket)

### 8.1 WebSocket Connection Management

```javascript
// WebSocket connection for real-time package updates
const packageSync = new WebSocket('wss://matter.darklang.com/sync');

// Authentication on connection
packageSync.send({
  type: 'authenticate',
  payload: {
    token: 'bearer-token',
    clientId: 'cli-client-123',
    subscriptions: [
      'packages.types.Darklang.Stdlib.*',
      'packages.functions.Darklang.Stdlib.List.*'
    ]
  }
});
```

### 8.2 Package Update Notifications

```json
{
  "type": "packageUpdated",
  "payload": {
    "packageId": "550e8400-e29b-41d4-a716-446655440000",
    "packageType": "Function",
    "name": {
      "owner": "Darklang",
      "modules": ["Stdlib", "List"],
      "name": "map"
    },
    "oldHash": "sha256:oldHash123",
    "newHash": "sha256:newHash456",
    "changeType": "Modified",
    "timestamp": "2025-08-10T11:15:00Z",
    "affectedDependents": [
      "550e8400-e29b-41d4-a716-446655440001",
      "550e8400-e29b-41d4-a716-446655440002"
    ]
  }
}
```

## 9. Security & Authentication Protocol

### 9.1 Authentication Flow

```http
# Initial authentication
POST /auth/login HTTP/1.1
Content-Type: application/json

{
  "username": "developer@darklang.com",
  "password": "secure-password",
  "clientType": "CLI"
}

# Response with JWT token
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expiresIn": 3600,
  "refreshToken": "refresh-token-123",
  "permissions": [
    "packages:read",
    "packages:search",
    "packages:subscribe"
  ]
}
```

### 9.2 Package Access Control

```fsharp
type PackagePermission =
  | ReadPublic          // Access to public packages
  | ReadPrivate         // Access to private/organizational packages
  | Write               // Ability to publish packages
  | Admin               // Full package management access

type AccessPolicy = {
  userId: String
  organizationId: Option<String>
  permissions: List<PackagePermission>
  packageScopes: List<String>  // Glob patterns for accessible packages
}
```

## 10. Error Handling & Recovery

### 10.1 Error Classification

```fsharp
type SyncError =
  // Network errors
  | NetworkTimeout of attemptCount: Int
  | ConnectionFailed of reason: String
  | ServerUnavailable
  
  // Authentication errors
  | AuthenticationFailed
  | PermissionDenied of packageId: String
  
  // Data integrity errors
  | HashMismatch of expected: String * actual: String
  | DeserializationFailed of packageId: String * error: String
  | DependencyNotFound of missingDep: String
  
  // Version conflicts
  | VersionConflict of conflictingVersions: List<String>
  | UnsupportedVersion of version: String
```

### 10.2 Recovery Strategies

```fsharp
module ErrorRecovery =
  
  let retryPolicy (error: SyncError) : RetryPolicy =
    match error with
    | NetworkTimeout count when count < 3 -> 
        RetryWithBackoff (exponentialBackoff count)
    | ConnectionFailed _ -> 
        RetryWithLinearBackoff 5
    | HashMismatch _ -> 
        InvalidateAndRefetch
    | AuthenticationFailed -> 
        ReAuthenticate
    | _ -> 
        NoRetry
```

## 11. Performance & Monitoring

### 11.1 Performance Metrics

```fsharp
type SyncMetrics = {
  // Throughput metrics
  packagesPerSecond: Float
  bytesPerSecond: Float
  compressionRatio: Float
  
  // Latency metrics  
  averageResponseTime: Duration
  p95ResponseTime: Duration
  p99ResponseTime: Duration
  
  // Cache metrics
  cacheHitRate: Float
  cacheSize: Int64
  evictionCount: Int64
  
  // Error metrics
  errorRate: Float
  timeoutRate: Float
  retryRate: Float
}
```

### 11.2 Telemetry Integration

```fsharp
module Telemetry =
  
  let recordPackageSync (packageId: String) (duration: Duration) (success: Bool) =
    OpenTelemetry.recordMetric "package_sync_duration" duration [
      ("package_id", packageId)
      ("success", string success)
    ]
    
  let recordCacheHit (packageId: String) (cacheLevel: String) =
    OpenTelemetry.incrementCounter "package_cache_hits" [
      ("package_id", packageId)  
      ("cache_level", cacheLevel)
    ]
```

## 12. Implementation Status & Roadmap

### 12.1 Current Implementation

✅ **Complete**
- Basic package search and discovery
- Local SQLite caching with RT columns
- HTTP-based package retrieval
- Custom binary serialization
- PackageManager PT/RT separation

🔄 **In Progress**  
- Incremental sync optimization
- WebSocket real-time updates
- Dependency resolution improvements

### 12.2 Future Roadmap

**Phase 1 (Q4 2025): Enhanced Sync**
- WebSocket-based real-time updates
- Improved compression algorithms
- Advanced caching strategies

**Phase 2 (Q1 2026): Distributed Sync**
- Peer-to-peer package distribution
- Multi-region package caching
- Offline-first synchronization

**Phase 3 (Q2 2026): Enterprise Features**
- Private package repositories  
- Organizational access control
- Audit logging and compliance

## Conclusion

This protocol specification establishes a robust, scalable foundation for package synchronization across the Darklang ecosystem. The content-addressable, hash-based approach ensures data integrity while supporting efficient incremental updates. The multi-runtime support (PT/RT) provides flexibility for different use cases, from development tooling to production execution environments.

The protocol's design prioritizes performance through multi-level caching, real-time updates via WebSocket, and optimized binary serialization. Security is ensured through JWT-based authentication and fine-grained access control. Comprehensive error handling and recovery mechanisms provide reliability in distributed environments.

---

**Implementation Files**: 
- `/backend/src/LibExecution/ProgramTypes.fs` - Core type definitions
- `/backend/src/LibPackageManager/PackageManager.fs` - Package management interface  
- `/packages/darklang/cli/` - CLI integration and caching
- Custom binary serialization system with RT/PT variants