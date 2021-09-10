#!/bin/bash
raco pollen render intro.tex.pm && xelatex -file-line-error intro.tex
