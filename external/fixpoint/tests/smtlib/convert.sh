#!/bin/bash

for f in *.fq; do 
  echo "Processing $f ..."; 
  ../../fixpoint.native -smtlib -out $f.smt $f
done

