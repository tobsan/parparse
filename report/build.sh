#!/bin/sh

REPORT="report.tex"
BIBL="report.aux"

while inotifywait -q $REPORT
do
    pdflatex $REPORT
    bibtex $BIBL
    pdflatex $REPORT
    pdflatex $REPORT
    echo "*********************************"
    echo "Regenerated pdf output from LaTeX"
    echo "*********************************"
done
