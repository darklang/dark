/// Where a run's random values come from: the OS, or a seeded generator for tests.
///
/// Only the values Dark code asks for (`Uuid.generate`, `Random.*`) come from here. The host's
/// own ids (store rows, package items, traces, processes) never do: they outlive a run.
module LibExecution.HostRandom

type Generator(seed : int) =
  let rng = System.Random(seed)

  member _.Fill(bytes : byte[]) : unit = lock rng (fun () -> rng.NextBytes bytes)

type Random =
  | Real
  | Seeded of Generator

/// A generator for a test run's seed, mixed with what tells this test apart, so each test's
/// values depend only on the seed and the test, not on what else ran first.
let seeded (seed : int64) (test : string) : Random =
  let digest =
    System.Security.Cryptography.SHA256.HashData(
      System.Text.Encoding.UTF8.GetBytes $"{seed}:{test}"
    )
  Seeded(Generator(System.BitConverter.ToInt32(digest, 0)))

let fill (random : Random) (bytes : byte[]) : unit =
  match random with
  | Real ->
    System.Security.Cryptography.RandomNumberGenerator.Fill(System.Span bytes)
  | Seeded s -> s.Fill bytes

let private withBits (version : byte) (bytes : byte[]) : System.Guid =
  bytes[6] <- (bytes[6] &&& 0x0Fuy) ||| version
  bytes[8] <- (bytes[8] &&& 0x3Fuy) ||| 0x80uy
  System.Guid(System.ReadOnlySpan bytes, true)

/// A v4 uuid (RFC 4122).
let uuidV4 (random : Random) : System.Guid =
  match random with
  | Real -> System.Guid.NewGuid()
  | Seeded _ ->
    let bytes = Array.zeroCreate<byte> 16
    fill random bytes
    withBits 0x40uy bytes

/// A v7 uuid (RFC 9562): <param unixMs>, then random bits.
let uuidV7 (random : Random) (unixMs : int64) : System.Guid =
  match random with
  | Real ->
    System.Guid.CreateVersion7(System.DateTimeOffset.FromUnixTimeMilliseconds unixMs)
  | Seeded _ ->
    let bytes = Array.zeroCreate<byte> 16
    fill random bytes
    for i in 0..5 do
      bytes[i] <- byte (unixMs >>> (8 * (5 - i)))
    withBits 0x70uy bytes
