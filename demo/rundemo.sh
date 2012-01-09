#!/bin/bash

CSOLVE="../src/csolve"

$CSOLVE "$1" > "$1.csolve.out"
CSOLVE_EXIT=$?

source-highlight --src-lang c --out-format html --line-number \
 --input "$1" --css=style.css --no-doc --output "$1.html"

./c2html.py "$1"

