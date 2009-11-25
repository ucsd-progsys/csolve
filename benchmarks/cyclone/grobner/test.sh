#!/bin/sh
#
# special test file for grobner, to deal with self-timing.
#

TMP=/tmp/grobner-test$$

if [ "$1" = "-k" ]; then
KILL=y
shift
else
KILL=
fi

if [ $# -lt 4 ]; then
  echo insufficient arguments
  exit 1;
fi

INFILE=$1
shift
OUTFILE=$1
shift
ERRFILE=$1
shift

# run the test, redirecting stdout to a tmpfile.

$* <$INFILE > $TMP 2>> $ERRFILE
EXITC=$?

# Then cat the tmpfile with the user-specified output file.

if [ -f "$TMP" ]; then
  if [ -z "$KILL" ]; then
    cat $TMP >> $OUTFILE
  else
    cat $TMP > $OUTFILE
  fi
fi

# Finally, extract the timing information

if [ $EXITC = 0 ]; then
  cat $TMP | grep TIME | awk '{ print $2; }'
fi

rm -f $TMP
exit $EXITC
