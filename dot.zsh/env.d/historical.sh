## Weird stuff that appeared over the years that I probably don't use.

# Fancy log output.  Pass log file name as input.  Highlights perl
# files and line nums, and indents stack trace.
log-tail () { 
    tail -f $1 |sed -re 's/\\n/\n\t/gp' \
        -e "s/^$|[0-9]+|[^ ]*\.p[^ ]*/`echo -ne '\e[31m'`&`echo -ne '\e[0m'`/g";
}

# Fancy highlighted cat
fancy-cat () { 
    enscript --color -w ansi -E -p- $1  | cat -n; 
}

# Interactive perl interpreter
iperl () { 
    perl -ne 'BEGIN { print ">> " }; print eval "$_"; print "\n>> "'
}

# make the xclock look super neat (TM).
## this was problematic because when i started the xclock from ions run menu
## it had default properties.  i set up properties for all instances in my 
## .Xdefaults file.
alias xclock="xclock -fg black -bg grey -hl red -update 1 -chime"

# grep -P may also work. be sure to escape "/"s ...
function perlgrep () { perl -ne "/$1/"' && print "$.:\t$_"' $2 }
