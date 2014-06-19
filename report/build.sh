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
    pdflatex -shell-escape "$REPORT"
    bibtex "$BIBL"
    pdflatex -shell-escape "$REPORT"
    pdflatex -shell-escape "$REPORT"
    echo "*********************************"
    echo "Regenerated pdf output from LaTeX"
    echo "*********************************"
done
