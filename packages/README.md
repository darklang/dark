# Darklang packages

This directory contains the Dark source for the standard library, CLI, SCM library, language tools, and package tests. Top-level directories group packages by owner;
module declarations inside `.dark` files give items their package names.

The development build loads these files into the local package store. A reload rebuilds the store's package definitions from this directory, so make repository
changes here rather than authoring them only in the development store.

Darklang is a local CLI. Packages are managed in a local store; this tree does not provide cloud storage, accounts, or synchronization between machines.

Dark has its own syntax and parser. Run `./scripts/run-cli docs for-ai` from the repository root for the language and workflow reference, or `./scripts/run-cli docs syntax` for syntax details.
