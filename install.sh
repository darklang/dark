#!/bin/sh
# Darklang CLI installer.
#
#   curl -fsSL https://wip.darklang.com/install | sh
#
# Installs the latest release to ~/.darklang/bin/dark and adds it to your PATH, after checking
# the download against the release's checksums file.
# Afterwards, the CLI manages itself (`dark update`, `dark uninstall`).
#
# To pin a release (tags at https://github.com/darklang/dark/releases):
#   curl -fsSL https://wip.darklang.com/install | DARKLANG_VERSION=vX.Y.Z sh
#
# darklang.com/install will serve this same file once the new site is live there; until then it
# 404s, so the URL above is the one to give people. Moving or renaming this file breaks it.

set -eu

say() { printf '%s\n' "$1"; }
fail() {
  printf 'darklang install: error: %s\n' "$1" >&2
  exit 1
}

# Append the given line to the given file unless that exact line already exists.
# Must stay byte-identical to what `dark update` writes, so neither tool
# duplicates the other's line.
append_line_once() {
  if [ -f "$1" ] && grep -qF "$2" "$1"; then
    return 0
  fi
  printf '\n%s\n' "$2" >>"$1"
}

is_musl() {
  [ -e /lib/ld-musl-x86_64.so.1 ] && return 0
  [ -e /lib/ld-musl-aarch64.so.1 ] && return 0
  command -v ldd >/dev/null 2>&1 && ldd --version 2>&1 | grep -qi musl
}

# The browser_download_url of the release asset whose name ends in $2, in release JSON $1.
asset_url_ending() {
  printf '%s' "$1" |
    grep -o "\"browser_download_url\"[[:space:]]*:[[:space:]]*\"[^\"]*$2\"" |
    head -n1 |
    sed 's/^"[^"]*"[[:space:]]*:[[:space:]]*"//; s/"$//'
}

# Extract the value of the first "field":"value" pair in a JSON string.
# $1 = json, $2 = field name
json_str_field() {
  printf '%s' "$1" |
    grep -o "\"$2\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" |
    head -n1 |
    sed 's/^"[^"]*"[[:space:]]*:[[:space:]]*"//; s/"$//'
}

main() {
  [ -n "${HOME:-}" ] || fail "\$HOME is not set"

  repo_api="https://api.github.com/repos/darklang/dark"
  darklang_home="$HOME/.darklang"
  bin_dir="$darklang_home/bin"

  if command -v curl >/dev/null 2>&1; then
    fetch() {
      curl -fsSL -H "accept: application/vnd.github+json" \
        -H "user-agent: darklang-installer" "$1"
    }
    download() { curl -fL --progress-bar -o "$2" "$1"; }
  elif command -v wget >/dev/null 2>&1; then
    fetch() {
      wget -qO- --header="accept: application/vnd.github+json" \
        --header="user-agent: darklang-installer" "$1"
    }
    download() { wget -qO "$2" "$1"; }
  else
    fail "curl or wget is required"
  fi

  if command -v sha256sum >/dev/null 2>&1; then
    sha256() { sha256sum "$1" | cut -d ' ' -f 1; }
  elif command -v shasum >/dev/null 2>&1; then
    sha256() { shasum -a 256 "$1" | cut -d ' ' -f 1; }
  else
    fail "sha256sum or shasum is required, to check the download"
  fi

  # Map OS/arch to the .NET runtime id used in release asset names
  os=$(uname -s)
  arch=$(uname -m)
  case "$os" in
    Linux)
      case "$arch" in
        # The musl build is fully static (OpenSSL included), so it runs on glibc systems too.
        x86_64 | amd64) rid="linux-musl-x64" ;;
        aarch64 | arm64)
          if is_musl; then
            fail "no prebuilt binary for musl-based Linux (e.g. Alpine) on arm64 yet"
          fi
          rid="linux-arm64" ;;
        armv7l)
          if is_musl; then
            fail "no prebuilt binary for musl-based Linux (e.g. Alpine) on armv7 yet"
          fi
          rid="linux-arm" ;;
        *) fail "unsupported Linux architecture: $arch" ;;
      esac ;;
    Darwin)
      case "$arch" in
        x86_64) rid="osx-x64" ;;
        arm64) rid="osx-arm64" ;;
        *) fail "unsupported macOS architecture: $arch" ;;
      esac ;;
    MINGW* | MSYS* | CYGWIN* | Windows_NT)
      fail "Dark does not support Windows. Linux and macOS only, including WSL." ;;
    *)
      fail "unsupported operating system: $os" ;;
  esac

  # Resolve the release. All Darklang releases are marked pre-release, so
  # GitHub's /releases/latest endpoint won't find them; take the newest
  # entry of the releases list instead (same approach as `dark update`).
  if [ -n "${DARKLANG_VERSION:-}" ]; then
    tag="$DARKLANG_VERSION"
    release_json=$(fetch "$repo_api/releases/tags/$tag") ||
      fail "couldn't find release '$tag' (see https://github.com/darklang/dark/releases)"
  else
    release_json=$(fetch "$repo_api/releases?per_page=1") ||
      fail "couldn't reach the GitHub API to look up the latest release"
    tag=$(json_str_field "$release_json" tag_name)
    [ -n "$tag" ] || fail "couldn't determine the latest release"
  fi

  # Suffix-match "-$rid.gz" so linux-arm can't match the linux-arm64 asset
  asset_url=$(asset_url_ending "$release_json" "-$rid\.gz")
  [ -n "$asset_url" ] || fail "release $tag has no prebuilt binary for $rid"
  asset_name=${asset_url##*/}
  sums_url=$(asset_url_ending "$release_json" "-sha256sums\.txt")
  [ -n "$sums_url" ] ||
    fail "release $tag has no checksums file, so its download can't be checked; nothing was installed"

  say "Installing the Darklang CLI ($tag, $rid) to $bin_dir/dark"

  tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/darklang-install.XXXXXX")
  new_binary="$bin_dir/.dark.new.$$"
  old_binary="$bin_dir/.dark.old.$$"
  had_old_binary=no
  install_pending=no

  cleanup() {
    status=$?
    trap - EXIT HUP INT TERM

    if [ "$install_pending" = yes ]; then
      rm -f "$bin_dir/dark"
      if [ "$had_old_binary" = yes ]; then
        if mv -f "$old_binary" "$bin_dir/dark"; then
          printf 'darklang install: restored the previous CLI after the failed upgrade\n' >&2
        else
          printf 'darklang install: error: could not restore the previous CLI from %s\n' "$old_binary" >&2
        fi
      fi
    else
      rm -f "$old_binary"
    fi

    rm -f "$new_binary"
    rm -rf "$tmpdir"
    exit "$status"
  }
  trap cleanup EXIT
  trap 'exit 1' HUP INT TERM

  # The checksum catches a download damaged or swapped on the way, or a binary replaced on the
  # release without its checksums file. TODO SECURITY: it doesn't authenticate the release; a
  # signature checked against a key pinned here would.
  download "$asset_url" "$tmpdir/dark.gz" || fail "download failed: $asset_url"
  download "$sums_url" "$tmpdir/sums.txt" || fail "download failed: $sums_url"
  expected=$(awk -v f="$asset_name" '$2 == f { print $1; exit }' "$tmpdir/sums.txt")
  [ -n "$expected" ] ||
    fail "release $tag's checksums file doesn't list $asset_name; nothing was installed"
  [ "$(sha256 "$tmpdir/dark.gz")" = "$expected" ] ||
    fail "$asset_name doesn't match the checksum release $tag lists for it, so it was damaged or changed on the way; nothing was installed. Please report this at https://github.com/darklang/dark/issues"
  gunzip "$tmpdir/dark.gz"

  mkdir -p "$bin_dir"
  mv "$tmpdir/dark" "$new_binary"
  chmod +x "$new_binary"

  # Smoke-test against an isolated home directory. CLI startup can migrate its
  # data store before dispatching `version`; pointing it at the user's live
  # ~/.darklang here would make a later executable rollback incomplete.
  test_home="$tmpdir/home"
  # The real ~/.darklang exists by now (bin/ is in it), so the smoke test's must too.
  mkdir -p "$test_home/.darklang"
  if ! HOME="$test_home" \
    XDG_CONFIG_HOME="$test_home/.config" \
    XDG_DATA_HOME="$test_home/.local/share" \
    "$new_binary" version >/dev/null 2>&1 </dev/null
  then
    fail "downloaded $tag, but running 'dark version' in an isolated environment failed; the existing installation was not changed. Please report this at https://github.com/darklang/dark/issues"
  fi

  # Keep a recoverable copy until the new executable is in place.
  if [ -e "$bin_dir/dark" ] || [ -L "$bin_dir/dark" ]; then
    cp -pP "$bin_dir/dark" "$old_binary"
    had_old_binary=yes
  fi

  install_pending=yes
  mv -f "$new_binary" "$bin_dir/dark"
  install_pending=no
  rm -f "$old_binary"

  case ":$PATH:" in
    *":$bin_dir:"* | *":$bin_dir/:"*) on_path=yes ;;
    *) on_path=no ;;
  esac

  # These lines must stay in sync with Stdlib.Cli.{Bash,Zsh,Fish}.addToPath;
  # $HOME is left unexpanded on purpose
  # shellcheck disable=SC2016
  path_line='export PATH="$HOME/.darklang/bin/:$PATH"'
  # shellcheck disable=SC2016
  fish_line='fish_add_path $HOME/.darklang/bin/'

  activate=""
  case "$(basename "${SHELL:-sh}")" in
    bash)
      append_line_once "$HOME/.bashrc" "$path_line"
      activate="source ~/.bashrc" ;;
    zsh)
      append_line_once "$HOME/.zshrc" "$path_line"
      activate="source ~/.zshrc" ;;
    fish)
      mkdir -p "$HOME/.config/fish"
      append_line_once "$HOME/.config/fish/config.fish" "$fish_line"
      activate="source ~/.config/fish/config.fish" ;;
    *)
      if [ "$on_path" = no ]; then
        say ""
        say "Add this to your shell's config file to put 'dark' on your PATH:"
        say "  $path_line"
      fi ;;
  esac

  say ""
  say "The Darklang CLI ($tag) is installed."
  if [ "$on_path" = yes ]; then
    say "Run 'dark' to get started."
  elif [ -n "$activate" ]; then
    say "Run '$activate' (or open a new terminal), then 'dark' to get started."
  fi
}

main "$@"
