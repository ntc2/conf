# from
# http://tips4linux.com/quick-and-easy-sharing-of-a-directory-over-http-in-linux/
# although Rigo showed this trick to once upon a time.
function nc:www-server () {
 : "usage $0 [DIR[/FILE]]"
 : "to start a webserver on port 8000 sharing DIR"
 (
     if [[ $# -eq 1 ]]; then
         if [[ -f "$1" ]]; then
             cd "$(dirname "$1")"
         elif [[ -d "$1" ]]; then
             cd "$1"
         else
             echo "error: not a path: $1" > /dev/stderr
             echo "usage:" > /dev/stderr
             type -f nc:www-server > /dev/stderr
             exit 2
         fi
     fi
     python -c 'import SimpleHTTPServer;SimpleHTTPServer.test()'
 )
}
