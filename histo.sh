#!/bin/bash

# function generate_histogram_image {
#     file=${1%.*}
#     echo "
# set terminal png
# set output \"${file}.png\"
# set datafile separator ','

# set title 'Histogram'
# set auto x
# set auto y
# set style data histogram
# # set style histogram cluster gap 1
# set style fill solid border -1
# set boxwidth 2
# #set bmargin 10
# set xtic rotate by 90

# plot [] [0:] \"${file}.csv\" using 2:xtic(1) ti col, \"\" using 3 ti col
# " | gnuplot
# }

# files=histo*.csv 

# for f in $files
# do
#     generate_histogram_image $f
# done

files=histo*.gn

for f in $files
do
    cat $f | gnuplot
done

echo -e '<html>\n'
echo -e '\t<head><title>Space-Invaders Histograms</title></head>'
echo -e '\t<body>'
echo -e '\t\t<h1>Histograms statistics from space-invaders</h2>'
for f in ${files//gn/png}
do
    echo -e "\t\t<h2>$f</h2>"
    echo -e "\t\t<img src=\"$f\" alt=\"$f\" />"
done

echo -e '\t</body>'
echo -e '</html>\n'