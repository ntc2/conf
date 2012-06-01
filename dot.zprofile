# The ~/.zprofile is only loaded for login shells, which should mean
# setting PATH here results in fewer duplicate entries than when
# setting in ~/.zshenv.

## Path
#
# If you want to be sure that path elements are unique, you can use
# `typeset -U`.  Seems to only work on scalars that are "tied".  I.e.,
# do:
#
#   FOO=...
#   typeset -TU FOO foo :
#
# Now 'foo' is an array consisting of 'FOO' split on ':', and both
# have had duplicates removed.  NB: a plain `typeset -U FOO` doesn't
# seem to work, altho my reading of `man zshbuiltins` indicates that
# it should :P

# !!!: Having '~/.cabal/bin' first here could be dangerous: a
# malicious package could install a rogue core util.  So, need to make
# a point of looking in '~/.cabal/bin' after running 'cabal install'
# ...
export PATH=~/.cabal/bin:~/local/opt/bin:~/local/bin:~/local/scripts:~/local/more-scripts:/opt/bin:$PATH
export MANPATH=~/local/opt/share/man:/opt/share/man:$MANPATH
# zsh looks for "functions" here, which includes completion functions
export FPATH=~/.zsh/completion:$FPATH
export PYTHONPATH=$HOME/local/scripts:$PYTHONPATH
