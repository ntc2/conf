# from
# http://tips4linux.com/quick-and-easy-sharing-of-a-directory-over-http-in-linux/
# although Rigo showed this trick to once upon a time.
function nc:www-server () {
 : "usage $0 [DIR]"
 : "to start a webserver on port 8000 sharing DIR"
 (
     if [[ $# -eq 1 ]]; then cd $1; fi
     python -c 'import SimpleHTTPServer;SimpleHTTPServer.test()'
 )
}
