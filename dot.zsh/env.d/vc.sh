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

function nc:git:disable-whitespace-conversion-on-repo {
  : 'usage: $0'
  :
  : 'Disable whitespace conversion for current repository.'
  echo '* text=unspecified' >> $(git rev-parse --git-dir)/info/attributes
}

function nc:git:disable-whitespace-conversion-on-one-file {
  : usage: $0 PATH
  :
  : Disable whitespace-conversion related attributes on PATH, locally
  : in the current repo.  "Absolute" path relative repo root is
  : safest.
  # The 'sed -re 's/ /?/' replaces spaces with single char wild card
  # to avoid this:
  # http://thread.gmane.org/gmane.comp.version-control.git/295351/focus=295389
  file=$(echo "$1" | sed -re 's/ /?/g')
  echo "$file -text -whitespace" >> $(git rev-parse --git-dir)/info/attributes
}

function nc:git:disable-whitespace-conversion-on-all-modified-files {
  : usage: $0
  :
  : Disable whitespace-conversion related attributes on all
  : currently modified files, locally in the current repo.
  # The 'xargs echo' removes quotes.
  (cd `git rev-parse --show-toplevel` && \
      git status --porcelain | grep '^ M' | sed -re 's/^ M //' | \
        xargs -n1 echo | \
        while read f; do
          nc:git:disable-whitespace-conversion-on-one-file "$f"
        done)
}

function nc:git:mirror {
  : 'usage: $0'
  :
  : 'Mirror the current Git repo.'
  nc:mirror:put $(git rev-parse --show-toplevel)
}
