#!/bin/bash

for f in $1/*.fq; do 
  echo "Processing $f ..."; 
  ../../fixpoint.native -smtlib -out $f.smt $f
  perl -pi -e 's/#/_/g' $f.smt
  ~/research/csolve/external/z3/bin/z3 $f.smt
done

