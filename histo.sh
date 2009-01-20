#!/bin/bash

function generate_histogram_image {
    file=${1%.*}
    echo "
set terminal postscript
set output \"${file}.ps\"
set datafile separator ','

set title 'Histogram'
set auto x
set auto y
set style data histogram
# set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 2
#set bmargin 10
set xtic rotate by -45

plot [] [0:] \"${file}.csv\" using 2:xtic(1) ti col
" | gnuplot # && gv ${file}.ps
}

for f in histo*.csv 
do
    generate_histogram_image $f
done