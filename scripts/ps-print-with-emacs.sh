#!/bin/bash

# use emacs ps-print-buffer-with-faces from the command line.
#
# based on
#
#   emacs --file {} --no-desktop \
#     --eval '(ps-print-buffer-with-faces)' --eval '(kill-emacs)'
#
# from "Batch" Printing With Faces section of
# http://www.emacswiki.org/emacs/PsPrint.
#
# prints 80 chars/line with no numbers, but these choices can be
# adjusted below.

if [[ $# != 1 ]]; then
    echo "usage: $0 FILE" > /dev/stderr
    exit 2
fi

in="$1"
out="/tmp/$(basename "$in").ps"
emacs \
    --quick \
    --file "$in" \
    --eval "
(let ((ps-top-margin 0)
;; adjust this and font size if enabling line numbers
      (ps-left-margin 0)
      (ps-right-margin 0)
      (ps-inter-column 0)
      (ps-landscape-mode t)
;;      (ps-paper-type 'ledger)
      (ps-number-of-columns 2)
;; 80 chars per line
      (ps-font-size 8.25)
;;      (ps-line-number t)
      (ps-line-number-font-size 10)
      (ps-line-number-step 10)
      (ps-print-color-p 'black-white))
  (ps-print-buffer-with-faces \"$out\")
;; gives chars per line info, among other things
;;  (ps-line-lengths)
;;  (write-file \"/tmp/line-lengths.txt\")
  (kill-emacs))
" 
echo "$in -> $out"

# you can get 2 pages per sheet with
#
#   psnup -d -n2 INFILE OUTFILE
#
# , but with wasted space, since american paper sizing is stupid. the
# rest of the world doesn't have this problem
# ... (http://en.wikipedia.org/wiki/Paper_size#The_international_standard:_ISO_216)

