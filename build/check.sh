#!/bin/sh
# checking the build
./build/clean.sh
R CMD check imptree
