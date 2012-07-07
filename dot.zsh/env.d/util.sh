# This modules is sourced before others.

function nc:load-custom {
    : usage: $0 FILE
    :
    : Load FILE if it exists.
    local FILE=$1
    if [[ -e $FILE ]]; then
	      source $FILE
    fi
}

function nc:usage {
    : 'usage: $0 FUNCTION [ERROR_MSG]'
    :
    : 'Print usage for FUNCTION and exit 2 (bad argument).'
    :
    : 'NB: the `exit` kills the calling shell, so this should'
    : 'only be used in sub shells. To use in a function that'
    : 'that does not spawn a sub shell, do:'
    :
    :     'return `(nc:usage ARGS)`'
    :
    : 'or'
    :
    :     'return `(nc:usage ARGS; echo $?)`'
    local fun="$1"
    local error_msg="$2"
    if [[ -n "$error_msg" ]]; then
        echo "error: $error_msg\n" > /dev/stderr
    fi
    type -f "$fun" > /dev/stderr
    exit 2
}

## Serialization

function nc:serialize {
    : 'usage: $0 LOCKFILE COMMAND'
    :
    : 'Run COMMAND using LOCKFILE as semaphore (not reentrant)'
    local lockfile="$1"
    shift

    # non-serialized example (doesn't count to 10):
    #
    # echo 0 > /tmp/count 
    # function inc () { n=$(cat /tmp/count); echo $((n+1)) > /tmp/count; }
    # for i in $(seq 1 10); do inc &; done
    # cat /tmp/count
    #
    # serialized example (does count to 10):
    #
    # echo 0 > /tmp/count 
    # function inc () { n=$(cat /tmp/count); echo $((n+1)) > /tmp/count; }
    # for i in $(seq 1 10); do nc:serialize lock inc &; done
    # cat /tmp/count

    # used to be able to use {mktemp} alone in the {while} below by
    # specifying the file name $lockfile, but now {mktemp} (since
    # becoming part of coreutils) won't allow you to specify the file
    # name exactly :P But, {link} is atomic in C according to
    # http://www.unix.com/high-level-programming/95946-atomic-lock-file-creation.html#post302274515
    # and so, guessing the shell command is also atomic, we get a link
    # based solution.  We're not allowed to link across devices, so we
    # create a temp file on the same file system as $lockfile :P
    dummy=$(mktemp -p $(dirname $lockfile))
    while ! link $dummy $lockfile &> /dev/null; do
        echo lockfile $lockfile found, sleeping ... >/dev/stderr
        sleep 1
    done
    "$@"
    rm -f $lockfile $dummy
}
