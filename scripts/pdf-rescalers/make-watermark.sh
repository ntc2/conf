#!/bin/zsh

# Usage: $0 <width in big points> <height in big points>
#
# Create a watermark pdf of the given width and height.

width="$1"
height="$2"

mkdir -p ~/tmp
prefix="$HOME/tmp/watermark-$width-$height"

# NB: Latex defines a 'pt' differently than a 'bp' (big point). We
# want big points.
cat <<EOF > "$prefix".tex
\documentclass{article}
\usepackage[paperwidth=${width}bp, paperheight=${height}bp]{geometry}
\input{watermark.tex}
EOF

TEXINPUTS="$(dirname "$0")/tex//:" pdflatex \
  -output-directory="$(dirname "$prefix")" \
  "$prefix".tex &>/dev/stderr

echo "$prefix".pdf
