#!/bin/bash
for file in *.gv
do
    name=${file%%.*}
    dot -Tsvg:cairo:cairo $name.gv > ../output/$name.svg
done
