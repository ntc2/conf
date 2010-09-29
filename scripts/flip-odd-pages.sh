#!/bin/bash

# flip the odd pages of a pdf
#
# useful for booklet printing, since i'm too stupid to get duplexing
# to flip on the short edge.
#
# acroread booklet procedure:
#
# print to file with 
# - Page Scaling = Booklet Printing
# - Booklet Subset = Both sides
# other settings don't seem to matter (e.g. both "portrait" and
# "landscape" will work) ... but be sure to reset "Page Scaling"
# before quitting (maybe by printing to file to /dev/null, since
# setting options and "cancel"ing does nothing :P)

if [[ $# -ne 2 ]]; then
    echo "usage: $0 INFILE OUTFILE" > /dev/stderr
    exit 2
fi
in="$1"
out="$2"

pdftk "$in" burst # creates page files called pg_%0.4d.pdf
max=`ls pg_*.pdf | sed -re 's/pg_0*(.*)\.pdf/\1/g' | sort -rn | head -n 1` # last page number
# flip odd pages
for n in `seq 2 2 $max`; do
    echo -ne "\rflipping page $n/$max"
    p=`printf "pg_%0.4d.pdf\n" $n`
    pdftk $p cat 1S output - > $p.tmp
    mv $p.tmp $p
done
pdftk pg_*.pdf cat output "$out" # rejoin pdf
rm pg_*.pdf
echo # there was no new-line on progress message