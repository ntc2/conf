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
