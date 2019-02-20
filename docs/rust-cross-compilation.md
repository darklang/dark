Rust cross compilation
======================

`dark-cli` is written in Rust so we have a single binary to give to users. As
such, we need to support all of OS X, Linux, and Windows.

Rust cross-compiles!  Unfortunately, not out of the box.

Much of the below comes from https://rwalk.xyz/rust-cross-compiling-from-ubuntu-to-os-x/

tl;dr (scripting & dockerification to come later):
- you need to download an XCode dmg (not xip). I believe `Xcode_7.3.1` is the
  latest release. You can get it from
https://developer.apple.com/services-account/download?path=/Developer_Tools/Xcode_7.3.1/Xcode_7.3.1.dmg,
and will need an Apple Developer account. (Free.)
- You will need github.com:tpoechtrager/osxcross. put `Xcode_7.3.1.dmg` in that
  directory, and run `./tools/gen_sdk_package_darling_dmg.sh Xcode_7.3.1.dmg`
- Put the resulting tar (tar.gz?) file in tarballs/ (dependencies: cmake,
  libxml2-dev, fuse).
- Run `OSX_VERSION_MIN=10.7 ./build.sh` # or `build_gcc.sh` for gcc, maybe? tbd.
- You'll need `osxcross/target/bin` in your PATH, and `.cargo/config` needs to
  contain:
[target.x86_64-apple-darwin]
linker = "x86_64-apple-darwin15-clang"
```
- At that point, 

NB: the dmg is enormous, like 6GB; so we're probs just gonna put the compiled
osxcross/target stuff in Docker, and create a separate Dockerfile that does
the above just so we can recreate/update that file if/when necessary.

It's 336M. Might be nice to make a tarball and put it somewhere so it's not in
our git repo? Or something? Will consider.

TODO:
- rustup target add x86_64-apple-darwin
  - need clang? or look at osxcross README.md on compiling with gcc
- rustup target add x86_64-windows-gnu
- it does seem like you need to build with --target individually, --all-targets
  doesn't behave as I'd expect
