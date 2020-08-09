#!/bin/sh

git rev-parse --short HEAD | \
    # Remove trailing endline
    tr -d '\n'
