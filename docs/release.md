# Releasing a new version of Darklang

Releasing a version of darklang is relatively simple.

## Tag the last commit of the month

- Find the commit: `git log | grep -B 5 "Merge pull request" | less`
- Tag the commit:

```
V=RELEASE_NUMBER && git tag -a release$V -m "Release $V - https://blog.darklang.com/darklang-release-$V/" COMMIT_SHA && git push origin release$V && git push origin release$V
```

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
