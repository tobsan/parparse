#!/bin/sh
#
# Small script to rebuild a pdf from LaTeX and BibTeX source automatically on
# file change. Requires the inotify-tools package.
#

REPORT="report.tex"
BIBL="report.aux"
REALBIBL="bibliography.bib"

while inotifywait -q "$REPORT" "$REALBIBL"
do
    sleep 1s
    pdflatex "$REPORT"
    bibtex "$BIBL"
    pdflatex "$REPORT"
    pdflatex "$REPORT"
    echo "*********************************"
    echo "Regenerated pdf output from LaTeX"
    echo "*********************************"
done
