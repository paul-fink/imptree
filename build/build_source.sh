#!/bin/bash
./build/clean.sh
rm imptree_*
R CMD build imptree
echo build R sources
