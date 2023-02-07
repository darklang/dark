## Setting up your editor

**Note that this doc is incomplete and sometimes may require tinkering after
doing the actions here. We welcome additions, clarifications, and instructions
on how to set up other editors.**

If you're using VSCode, see instead [docs/vscode-setup.md](docs/vscode-setup.md).

### Formatting

You will also want to support formatting in your client. Dark uses Prettier for
JS/HTML/CSS and Fantomas for F#. The script
`script/formatting/format` can be used to format or check formatting, and there is a
pre-commit hook you can use to run it automatically.

### Pre-commit hook

You probably also want to install a pre-commit hook that the formatters for
you.
`cp scripts/formatting/pre-commit-hook.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit`
