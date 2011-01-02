# Interpret ascii escapes and show verbose status and ignore case in searches
alias less="less -RMi"

# get syntax highlighting in less, like on gentoo (doesn't work with
# pipe though :P)
#
# to get original less.vim script do
#
#   $ sudo aptitude install vim-runtime vim-scripts (or vim-runtime vim-nox)
#
# the one in conf/lib is slightly modified (lines added at the end).
alias vless='vim -u ~/v/conf/lib/less.vim'

# standard
if which lesspipe &>/dev/null; then
    eval $(lesspipe)
# Gentoo
elif which lesspipe.sh &>/dev/null; then
    export LESSOPEN="|lesspipe.sh %s"
else
    message "Unable to setup LESSOPEN filter."
fi

