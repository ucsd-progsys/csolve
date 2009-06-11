#!/usr/bin/perl -w
# translate away vararg function calls to 'xlslave'

while ($line = <STDIN>) {
  # is this line a call to xlsave?
  if ($line !~ /xlsave/) {
    # no, emit unchanged
    print ($line);
    next;
  }

  # try to extract the componnents
  ($lhs, $arglist) = ($line =~ /^([^=]+)= xlsave\((.+)\);\s*$/);
  if (!( $lhs && $arglist )) {
    # doesn't match our pattern
    print STDERR ("nonconform: $line");
    print ($line);
    next;
  }

  # parse the arguments
  @args = split(',', $arglist);
  if ($args[$#args] !~ /NULL/) {
    print STDERR ("non-null last arg: $line");
    print ($line);
    next;
  }

  # remove last argument
  $#args = $#args - 1;

  # compute the name of the function to call
  $func = "xlsave" . ($#args + 1);

  # and the argument list
  $newarglist = join(',', @args);

  # emit the modified line
  print ("$lhs= $func($newarglist);\n");
}



