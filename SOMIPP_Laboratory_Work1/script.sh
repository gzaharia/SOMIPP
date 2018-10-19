#!/bin/bash
x="$1"
y="${x%.*}"
STR="$(stat --printf="%s" $1)"
truncate -s "$((1474560-$STR))" file.txt
cat misa.txt>>$1
mv $1 "$y.img"
rm file.txt