#!/bin/bash
./build/clean.sh
rm imptree.source.tar.gz
tar -cvzf imptree.source.tar.gz imptree
echo build plain source archive  
