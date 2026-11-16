// Brotli-compress the seed for embedding: `dotnet fsi compress-seed.fsx <in> <out>`.
//
// A script rather than the `brotli` tool because every release host already has the .NET SDK and
// none has `brotli`. A 16 MB window (24): the seed repeats the same hashes and names far apart,
// which a large window finds. Quality 9, not 11: 11 is ~6% smaller but takes ~7x as long.
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
      9,
      24
    )
  )
then
  failwith "Brotli compression of the seed failed"

File.WriteAllBytes(fsi.CommandLineArgs[2], output[.. written.Value - 1])
