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

function nc:git:disable-whitespace-conversion-on-all-modified-files {
  : usage: $0
  :
  : Disable whitespace-conversion related attributes on all
  : currently modified files, locally in the current repo.

  (cd `git rev-parse --show-toplevel` && \
   git st -s | grep '^ M' | awk '{print $2}' | \
   while read f; do
     nc:git:disable-whitespace-conversion $f
   done)
}

function nc:git:mirror {
  : 'usage: $0'
  :
  : 'Mirror the current Git repo.'
  nc:mirror $(git rev-parse --show-toplevel)
}
