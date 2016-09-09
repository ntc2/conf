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

################################################################
# Dynamic directory for current Git repo.
#
# Makes `~[r]` in paths resolve to current Git repo root.
#
# This is a slightly modified version of a snippet Daniel Wagner
# posted in the Galois MatterMost.

# see "Dynamic named directories" in man zshexpn for an explanation of how
# these functions are used
zsh_directory_name_functions+=(dyndir_git_repo)
# I prefer that zsh print full paths to my repos, but you can request that
# it replace paths with ~[repo] by changing this to, e.g., true
dyndir_git_repo_abbreviate=false

# git repos
dyndir_git_repo() {
    case "$1" in
        n)
            [ "$2" = "r" ] && \
            reply=(`git rev-parse --show-toplevel 2>/dev/null`)
            ;;
        d)
            # Why not git rev-parse --show-toplevel? Because it doesn't handle
            # paths with symlinks gracefully: --show-toplevel always shows the
            # absolute path, but we want to replace a potentially indirect
            # path.
            #
            # The double-quotes in cd "`git ...`" are needed so that it
            # doesn't turn into a bare cd command when in the repository root.
            local cmd='cd "'"$2"'" && cd "`git rev-parse --show-cdup 2>/dev/null`" && pwd'
            $dyndir_git_repo_abbreviate && \
            # subshell used for two facets:
            # 1. it won't load .zshrc, hence won't recurse into this function
            #    when changing directory
            # 2. the directory change won't affect the current shell
            reply=("repo" `zsh -c "$cmd" | wc -L`)
            ;;
        c)
            # TODO: understand how completion works; see man zshcompsys
            # proposed behavior: add repo to the list of completions if we're
            # currently in a git repo
            return 1;;
        *) echo unsupported query "$1" in "$0" >&2;;
    esac
}
