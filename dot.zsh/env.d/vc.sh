## Subversion

function nc:svn:lsjunk {
  : usage: $0 [PATH]
  :
  : Show untracked non-ignored files PATH, default current dir.
  :
  : Use with '| xargs rm' to delete junk.
  svn st "$@" | grep '^\?' | awk '{print $2}'
}
