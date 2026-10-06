# Darklang

This is the main repo for [Darklang](https://darklang.com), a combined language, editor,
and infrastructure to make it easy to build backends and CLIs.

[![Ceasefire Now](https://badge.techforpalestine.org/ceasefire-now)](https://techforpalestine.org/learn-more)

This repo is intended to help Darklang users solve their needs by fixing bugs,
expanding features, or otherwise contributing. Darklang is [open source](https://blog.darklang.com/TODO)
under the Apache License 2.0. See our [LICENSE.md](https://github.com/darklang/dark/blob/main/LICENSE.md).

Note that the production version of Darklang, ["Darklang-Classic"](https://github.com/darklang/classic-dark),
is not in this repo. Since Feb 2023, the Darklang team has been working on a new version of Darklang,
which is in this repo -- temporarily, we're referring to this as "dark-next".
Dark-next isn't yet ready for production use.

## Install

On Linux or macOS:

```
curl -fsSL https://wip.darklang.com/install | sh
```

That puts `dark` in `~/.darklang/bin` and adds it to your PATH.

Or by hand, which is the way on Windows: download the file for your platform from the
[latest release](https://github.com/darklang/dark/releases) (the release notes say which is
which), decompress it, and run it from wherever you put it. On Linux x64:

```
gunzip darklang-alpha-*-linux-x64.gz
chmod +x darklang-alpha-*-linux-x64
./darklang-alpha-*-linux-x64
```

A fresh install can't touch files, the network, processes or environment variables until you
allow it; the refusal names the `dark permissions allow ...` command that does.

See also:

- The [Discord](https://darklang.com/discord-invite), where most of our communication happens - join and say hi!
- Our [GitHub Issues](https://github.com/darklang/dark/issues), where most work is tracked
- Darklang-Classic [login](https://darklang.com/login) and [repo](https://github.com/darklang/classic-dark)
- A [guide](/CONTRIBUTING.md) for contributing to Dark
- Our [guide to the repo](https://docs.darklang.com/contributing/repo-layout) for help browsing -- though, it's a bit outdated
