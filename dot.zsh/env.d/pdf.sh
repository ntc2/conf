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

which pdftk &>/dev/null || return `(nc:usage $0 "You need to install pdftk")`
[[ $# -eq 1 ]] && [[ -e "$1" ]] || return `(nc:usage $0)`
local file="$1"

# Doing everything in ~/tmp instead of /tmp to work around bug in
# pdftk Snap package on Ubuntu:
# https://github.com/smoser/pdftk/issues/1
mkdir -p ~/tmp
cp "$file" ~/tmp/
file=~/tmp/$(basename "$file")

local wmfile=$(nc:pdf:watermark "$file")
local nupfile=~/tmp/$(basename "$file").pdfnup.pdf
evince "$wmfile" &>/dev/null &!

local left bottom right top nup
echo -n '<top> <left> <right> <bottom> (trims in cm): '
read top left right bottom
echo -n '<nup> (probably 2x1 or 1x1): '
read nup
pdfinfo "$file"
echo -n '<paper> (default: "letter"; other option: "a4paper") '
read paper
if [[ -z "$paper" ]]; then
  paper=letter
fi
# Insert pdfnup command into command line without running it.
print -z \
"pdfnup --paper $paper --nup $nup \
--trim \"${left}cm ${bottom}cm ${right}cm ${top}cm\" \
--outfile \"$nupfile\" \"$file\" \\\\
&& evince \"$nupfile\""
}

nc:pdf:watermark () {
: usage: $0 FILE
:
: Apply a watermark to a pdf, to help calculate pdfnup trim params.

local file="$1"

local size="$(pdfinfo "$file" | grep 'Page size:')"
local  width=$(echo "$size" | sed -nre 's/.*: *([.0-9]+) x ([.0-9]+) pts.*/\1/p')
local height=$(echo "$size" | sed -nre 's/.*: *([.0-9]+) x ([.0-9]+) pts.*/\2/p')

local watermark="$(~/v/conf/scripts/pdf-rescalers/make-watermark.sh $width $height)"
mkdir -p ~/tmp
local watermarked=~/tmp/$(basename "$file").wm
# Prefer 'stamp' to 'background' because it works even for PDFs with
# opaque backgrounds.
pdftk "$file" \
  stamp "$watermark" \
  output "$watermarked"

echo "$watermarked"
}
