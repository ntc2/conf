#!/bin/zsh

# Show diffs for all files with 'git update-index --skip-worktree'
# set.
#
# Helper script for 'git ignore-changes-diff' alias. When I tried to
# implement this directly in ~/.gitconfig I kept getting "fatal: bad
# config file line ..." errors.

vless <(
  git ignore-changes-ls | while read file; do
    diff -u <(git cat-file blob "HEAD:$file") "$(git rev-parse --show-toplevel)/$file"
  done
)
