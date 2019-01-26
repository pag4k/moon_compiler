#!/bin/bash
shopt -s nullglob
for myfile in *.gv
do
  dot -Tpng $myfile -o ${myfile/.gv/.png}
done
