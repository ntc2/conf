# Based on
# http://tips4linux.com/quick-and-easy-sharing-of-a-directory-over-http-in-linux/
# although Rigo showed this trick to once upon a time.
#
# XXX: mimetype handling is overly complex.  The automatic
# determination via 'file' is probably sufficient, making the option
# parsing unnecessary, but I figured out the option version first so
# I'm leaving it in for now :P
#
# XXX: may be useful to set mimetypes for DIR too, e.g.
#
#   $0 -d "c,hs,sh" DIR
#
# to share DIR with .c, .hs, and .sh files having mimetype text/plain.
function nc:www-server () {
 : 'usage: $0 [DIR | [-m MIMETYPE | -t] FILE]'
 :
 : 'Start a webserver on port 8000 sharing DIR or FILE.'
 : 
 : 'When sharing FILE, optionally set mimetype.'
 : 'If not set, then mimetype is determined using `file`.'
 :
 : 'NB: when sharing a FILE, the parent (`dirname FILE`) is *not* shared.'
 (
     local o
     # See `zman getopts`.  NB: leading ':' needed to get '?' in '$o'
     # for bad options.
     while getopts :tm: o
     do  case "$o" in
             t) mimetype="text/plain";;
             m) mimetype="$OPTARG";;
             ?) nc:usage nc:www-server "bad option: $o";;
         esac
     done
     shift $OPTIND-1
     local set_mimetype="pass"

     [[ $# -eq 1 ]] || nc:usage nc:www-server "bad path"

     local file="$1"
     # Create temp dir containing only FILE and share that.
     if [[ -f "$1" ]]; then
         # Determine mimetype.
         #
         # It's useful to set mimetype so that file can be
         # displayed in browser, instead of forcing download.
         if [[ -z "$mimetype" ]] && $(which file &>/dev/null); then
             mimetype="$(file --brief --mime-type "$file")"
         fi
         if [[ -n "$mimetype" ]]; then
             local ext="${f##*.}"
             local set_mimetype="SimpleHTTPServer.SimpleHTTPRequestHandler.extensions_map['.$ext']='$mimetype'"
         fi
         echo "Using mimetype: $mimetype"
         
         local tmpdir="$(mktemp -d)"
         local full_path="$(readlink -f "$1")"
         cd "$tmpdir"
         ln -s "$full_path" ./
     elif [[ -d "$1" ]]; then
         cd "$1"
     else
         nc:usage nc:www-server "does not exist: $file"
     fi

     python -c "import SimpleHTTPServer; \
                $set_mimetype; \
                SimpleHTTPServer.test()"
 )
}
