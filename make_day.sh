#!/bin/bash 

mkdir $1 
cd $1 
touch input.txt 
touch dune 
touch dune-project
cp ../day2/.ocamlformat .
touch $1.ml