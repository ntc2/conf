#!/bin/bash

USAGE="usage: $0 DIR"

# Resize all images in DIR to 1200x900.  Resized imaged have same name
# as originals, but are saved in DIR.SIZE, where SIZE is the size
# rescaled to.

if [[ $# -ne 1 ]]; then
    echo $USAGE >&2
fi

SIZE=1024x768
DIR="$1"
OUT="$DIR.$SIZE"

mkdir "$OUT"

# this handles names with internal spaces, but not leading or trailing
# spaces or some other weird stuff says internet
find $DIR -type f | while read f; do
    echo "Converting $f"
    convert -resize $SIZE "$f" "$OUT/$(basename "$f")"
done
