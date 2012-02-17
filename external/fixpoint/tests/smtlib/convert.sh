#!/bin/bash

for f in $1/*.fq; do 
  echo "Processing $f ..."; 
  ../../fixpoint.native -smtlib -out $f.smt $f
done

