# Interpret ascii escapes and show verbose status and ignore case in searches
alias less="less -RMi"

# standard
if which lesspipe &>/dev/null; then
    eval $(lesspipe)
# Gentoo
elif which lesspipe.sh &>/dev/null; then
    export LESSOPEN="|lesspipe.sh %s"
else
    message "Unable to setup LESSOPEN filter."
fi

