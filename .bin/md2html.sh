#!/bin/sh

pandoc -f gfm -t html -s -H ~/.config/pandoc/modest.css $1 -o $2

