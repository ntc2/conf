## Subversion

function nc:svn:ls-junk {
  : usage: $0 '[PATH]'
  :
  : Show untracked non-ignored files PATH, default current dir.
  :
  : Use with '| xargs rm' to delete junk.
  svn st "$@" | grep '^\?' | awk '{print $2}'
}

## Git

# See ../completion/README for completion.

function nc:git:disable-whitespace-conversion {
  : usage: $0 PATH
  :
  : Disable whitespace-conversion related attributes on PATH, locally
  : in the current repo.  "Absolute" path relative repo root is
  : safest.

  echo "$1 -text -whitespace" >> $(git rev-parse --git-dir)/info/attributes
}
