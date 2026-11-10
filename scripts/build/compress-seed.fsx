// Brotli-compress the seed for embedding: `dotnet fsi compress-seed.fsx <in> <out>`.
//
// A script rather than the `brotli` tool because every release host already has the .NET SDK and
// none has `brotli`. Quality 11 with a 16 MB window (24): the seed repeats the same hashes and
// names far apart, which a large window finds. 7.15 MB with `gzip -9`, 5.28 MB this way.
open System.IO
open System.IO.Compression

let input = File.ReadAllBytes fsi.CommandLineArgs[1]
let output = Array.zeroCreate<byte> (BrotliEncoder.GetMaxCompressedLength input.Length)
let written = ref 0

if
  not (
    BrotliEncoder.TryCompress(
      System.ReadOnlySpan input,
      System.Span output,
      &written.contents,
      11,
      24
    )
  )
then
  failwith "Brotli compression of the seed failed"

File.WriteAllBytes(fsi.CommandLineArgs[2], output[.. written.Value - 1])
