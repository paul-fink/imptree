#!/bin/sh
# changing into the main directory
rm -rf imptree.Rcheck
rm -rf imptree
mkdir imptree
cp DESCRIPTION imptree
cp NAMESPACE imptree
cp ChangeLog imptree
mkdir imptree/data
cp data/car_acceptance.rda imptree/data
mkdir imptree/man
cp man/accuracy.Rd imptree/man
cp man/car_acceptance.Rd imptree/man
cp man/impbag.Rd imptree/man
cp man/imptree.Rd imptree/man
cp man/imptree_control.Rd imptree/man
cp man/predict.Rd imptree/man
cp man/predclass.Rd imptree/man
mkdir imptree/src
cp src/entropyIDM.c imptree/src
cp src/entropyNPI.c imptree/src
cp src/predict.c imptree/src
cp src/utils.c imptree/src
cp src/utils.h imptree/src
mkdir imptree/R
cp R/accuracy.r imptree/R
cp R/checkParam.r imptree/R
cp R/entropyIDM.r imptree/R
cp R/entropyNPI.r imptree/R
cp R/imptree_control.r imptree/R
cp R/predclass.r imptree/R
cp R/predict.r imptree/R
cp R/print.r imptree/R
# cp R/testing.r imptree/R
cp R/treebuilder.r imptree/R
cp R/utils.r imptree/R

echo clean done
