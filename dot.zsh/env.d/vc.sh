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
     nc:git:disable-whitespace-conversion-on-one-file $f
   done)
}

function nc:git:mirror {
  : 'usage: $0'
  :
  : 'Mirror the current Git repo.'
  nc:mirror:put $(git rev-parse --show-toplevel)
}

function nc:git:clone {
  : 'usage: $0 GIT_CLONE_URL [PARENT]'
  :
  : 'Clone a git repo into parent dir PARENT (default .) with repo dir'
  : 'derived from the GIT_CLONE_URL, *with* the .git at the end.'
  :
  : 'For example:'
  :
  :   '$0 user@www.example.com:user/path/to/repo.git parent'
  :
  : 'will clone into'
  :
  :   'parent/repo.git'
  if (($# < 1 || $# > 2)); then
    return $(nc:usage nc:git:clone "Wrong number of arguments.")
  fi

  url="$1"
  repo="$(basename "$url")"
  if (($# == 1)); then
    parent="."
  else
    parent="$2"
  fi

  mkdir -p "$parent"
  git clone "$url" "$parent"/"$repo"
}
