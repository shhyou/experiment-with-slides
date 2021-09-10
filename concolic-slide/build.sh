#!/bin/bash
/usr/bin/time -p raco make -v main.tex.pm && \
PLTSTDOUT='info@markdown-enriching' \
  /usr/bin/time -p raco pollen render main.tex.pm && \
  xelatex -file-line-error main.tex && \
  xelatex main.tex
