# Releasing a new version of Darklang

Main doesn't release on merge. To release its newest commit, run this from anywhere you can push to darklang/dark:

```
scripts/deployment/release
```

It checks that CI passed on that commit, shows it with the next `v0.0.N`, and pushes that tag once you confirm. The tag starts CircleCI's `release-clis` workflow, which builds every platform and publishes the GitHub release. Any tag starting with `v` does the same, so pushing one by hand works too.

## Create the changelog

In the docs repo, update changelog.md.

You can automatically generate changelog entries from PRs using:

```
pip install github-changelog
brew install gnu-sed
changelog darklang dark releasePREV_RELEASE_NUMBER releaseRELEASE_NUMBER | sed 's!#\([[:digit:]]\+\)![#\1]\(https://github.com/darklang/dark/pull/\1\)!g'
```

## Release materials

- Blog post
- Twitter @darklang
- Reflection recording on youtube
- Announcement channel on Discord
- Send email via mailchimp
