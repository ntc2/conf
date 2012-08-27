nc:pdf:tutorial () {
: Remind how to trim a pdf.  Optional arguments fill in some parts of generated commands.
:
: Use nc:pdf:magic instead!
local watermark=${1-\$watermark} file="${2-\$file}"
cat <<EOF
usage: $0 [WATERMARK [FILE]]

USE nc:pdf:magic INSTEAD!

See ~/v/conf/scripts/pdf-rescalers/README.org for more details.

pdfinfo "$file"
nc:pdf:watermark $watermark "$file"
evince "$file".wm
# left bottom right top
pdfnup --paper $watermark --nup 1x1 --trim "1.5cm 4.5cm 1.5cm 0cm" "$file"
EOF
}

nc:pdf:magic () {
: usage: $0 FILE
:
: Guess watermark, apply watermark, open watermarked file, 
: and insert imcomplete 'pdfnup' command.
[[ $# -eq 1 ]] && [[ -e "$1" ]] || return `(nc:usage $0)`
local file="$1"

local wm=$( pdfinfo "$file" \
          | grep 'Page size:' \
          | sed -re 's/.*\((.*?)\).*/\1/')

local wmfile=$(nc:pdf:watermark $wm "$file")
local nupfile=/tmp/$(basename "$file").pdfnup.pdf
evince "$wmfile" &>/dev/null &!

local left bottom right top nup
echo -n '<left> <bottom> <right> <top> (trims in cm): '
read left bottom right top
echo -n '<nup> (probably 2x1 or 1x1): '
read nup
# Insert pdfnup command into command line without running it.
print -z \
"pdfnup --paper $wm --nup $nup \
--trim \"${left}cm ${bottom}cm ${right}cm ${top}cm\" \
--outfile \"$nupfile\" \"$file\" \\\\
&& evince \"$nupfile\""
}

nc:pdf:watermark () {
: usage: $0 WATERMARK FILE
:
: Apply a watermark to a pdf, to help calculate pdfnup trim params.
local watermark=$1 file="$2"
local wmfile=/tmp/$(basename "$file").wm
pdftk "$file" \
  background ~/v/conf/scripts/pdf-rescalers/watermark-$watermark.pdf \
  output "$wmfile"
  echo "$wmfile"
}
