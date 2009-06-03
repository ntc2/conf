
# Not strictly SSH related, but motivated by slow tab completion (if
# at all) over ssh.

function _nc:find () {
    : "$0 DIR PAT: list non dot files in DIR matching PAT"
    find "$1" -maxdepth 1 -not -name '.*' -name "${2:-*}"
}

function nc:enumerate () {
    : "$0 DIR PATTERN: find names in DIR matching PATTERN and enumerate them"
    _nc:find "$@" | cat -n
}

function nc:nth () {
    : "$0 $@: select Nth file of nc:enumerate \"$@\""
    local N="$1"
    shift
    _nc:find "$@" | sed -ne "${N}p"
}
