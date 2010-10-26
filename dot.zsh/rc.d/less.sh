# Interpret ascii escapes and show verbose status and ignore case in searches
alias less="less -RMi"

# get syntax highlighting in less, like on gentoo (doesn't work with
# pipe though :P)
#
# sudo aptitude install vim-runtime vim-scripts (or vim-runtime vim-nox)
alias vless='vim -u /usr/share/vim/vimcurrent/macros/less.vim'

# standard
if which lesspipe &>/dev/null; then
    eval $(lesspipe)
# Gentoo
elif which lesspipe.sh &>/dev/null; then
    export LESSOPEN="|lesspipe.sh %s"
else
    message "Unable to setup LESSOPEN filter."
fi

