#!/usr/bin/perl -w

#
# perfsuite.pl
#
# This script runs Deputy over a number of tests
# and records the time spent in each optimization phase,
# the number of runtime checks remaining in each test,
# and the performance of each test.
#
#
# Specify -r to run the tests.
# Specify -B to set the most recent run as the baseline.
# Specify -o num to set the optimization level
# Run without options to compare the most recent run to
#  the baseline.

use strict;

my $rt = 0;   # should the tests be rerun?
my $base = 0; # should a new baseline be made?
my $help = 0; # should a usage message by printed?
my $opt = 4;  # optimization level
my $patch = 0;
my $popt = "";

my($title,$name,$number);
format STDOUT =
@<<<<<<<<<<  @<<<<<<<<<<<<<<<<<<<<  @>>>>>>>>>
$title,       $name,                 $number
.


# Array of test names and source directories.
# The first element is the argument to make,
# and the second says in which source files to
# look for Deputy checks.
# Lines commented out indicate that a test is broken.
my @Tests =
  (
   ["gzip",      "spec00/gzip/src/*.cil.c"],
   ["go",        "spec95/go/src/*.cil.c"],
#   ["ijpeg",     "spec95/ijpeg/src/*.cil.c"],
   ["li",        "spec95/li/src/*.cil.c"],
   ["bh",        "olden/bh/*.cil.c"],
   ["em3d",      "olden/em3d/*.cil.c"],
   ["health",    "olden/health/*.cil.c"],
   ["mst",       "olden/mst/*.cil.c"],
   ["treeadd",   "olden/treeadd/*.cil.c"],
#   ["perimeter", "olden/perimeter/*.cil.c"],
   ["tsp",       "olden/tsp/*.cil.c"],
   ["power",     "olden/power/*.cil.c"],
   ["bisort",    "olden/bisort/*.cil.c"],
   ["anagram",   "ptrdist/anagram/*.cil.c"],
   ["bc",        "ptrdist/bc/*.cil.c"],
   ["ft",        "ptrdist/ft/*.cil.c"],
   ["ks",        "ptrdist/ks/*.cil.c"],
   ["yacr2",     "ptrdist/yacr2/*.cil.c"],
   ["epic",      "mediabench/epic/src/*.cil.c"],
   ["adpcm",     "mediabench/adpcm/src/*.cil.c"]
#   ["perftest/matxmult", "perf/matxmult.cil.c"],
#   ["perftest/matxmult2", "perf/matxmult2.cil.c"],
#   ["perftest/array1", "perf/array1.cil.c"],
#   ["perftest/array2", "perf/array2.cil.c"],
#   ["perftest/array3", "perf/array3.cil.c"],
#   ["perftest/array_incr", "perf/array_incr.cil.c"],
#   ["perftest/string1", "perf/string1.cil.c"]
  );

my @Checks =
  (
   "CNonNull",
   "CEq",
   "CMult",
   "CPtrArith",
   "CPtrArithNT",
   "CPtrArithAccess",
   "CLessInt",
   "CLeqInt",
   "CLeq",
   "CLeqNT",
   "CNullOrLeq",
   "CNullOrLeqNT",
   "CWriteNT",
   "CNullUnionOrSelected",
   "CSelected",
   "CNotSelected",
  );

# Phases of the optimizer for which timing information
# is emitted by deputy when --deputystats is specified
# on the command line. Follow the pattern in doptimmain.ml
# to see how to get timing information from a phase.
my @OptimPhases =
  (
   "flow-insensitive",
   "matts",
   "dup-check-elim",
   "symbol-eval",
   "constant-fold",
   "loopcheck",
   "loopcheck-cleanup",
   "check-merge",
   "dce",
   "check-hoist",
   "oct-analysis",
   "TOTAL",
   "PreFIChecks",
   "PostFIChecks"
  );

my %TimesTotal;
my %CheckTotal;
my %TestTimes;
my %BaseTimes;
my $testTimeTotal = 0;

sub runTests {
  foreach my $test (@Tests) {
    my $name = $test->[0];
    print STDOUT "Building and running $name\n";
    `make $name OPT=$opt $popt EXTRAARGS=\"--stats --do-ptr-analysis --prop-preconditions\" &> $name.perfsuite.txt`;
  }
}

sub runBaseTests {
  foreach my $name (@Tests) {
    print STDOUT "Building and running $name->[0]-base\n";
    `make $name->[0]-base &> $name->[0]-base.perfsuite.txt`;
  }
}

sub getGlobalChecks {
  my $test = shift;
  my $ores = 0;
  my $hres = 0;

  open RESFILE, "<$test->[0].perfsuite.txt"
    or die "Could not open $test->[0].perfsuite.txt";

  while(<RESFILE>) {
    my $line = $_;
    $line =~ /^(\S*):\s*(\d*)\s*(\d*)/;
    if(defined $1 and defined $2 and defined $3 and $1 eq "GlobalChecks") {
      print STDOUT "found GlobalCheck = $2 $3\n";
      $ores = $ores + $2;
      $hres = $hres + $3;
    }
  }

  close RESFILE;

  return ($ores, $hres);
}

sub getFieldSum {
	my $test = shift;
	my $fname = shift;
	my $res = 0;

  open RESFILE, "<$test->[0].perfsuite.txt"
    or die "Could not open $test->[0].perfsuite.txt";

  while(<RESFILE>) {
    my $line = $_;
    $line =~ /^(\S*)\s*(\d*)/;
    if(defined $1 and defined $2 and $1 eq $fname) {
      $res = $res + $2;
    }
  }

  close RESFILE;

  return $res;
}

sub getComplicatedChecks {
  my $test = shift;
  my $res = 0;

  open RESFILE, "<$test->[0].perfsuite.txt"
    or die "Could not open $test->[0].perfsuite.txt";

  while(<RESFILE>) {
    my $line = $_;
    $line =~ /^(\S*)\s*(\d*)/;
    if(defined $1 and defined $2 and $1 eq "octoInsuf") {
      $res = $res + $2;
    }
  }

  close RESFILE;

  return $res;
}

sub reportChecks {
  my $test = shift;
  my $total = 0;
  my $ocount;
  my $hcount;

  foreach my $check (@Checks) {
    my $num = `grep \"$check\(\" $test->[1] | wc -l `;
    chomp($num);
    if( $num > 0 ) {
      $title = $test->[0];
      $name  = $check;
      $number = $num;
      write STDOUT;
      if( defined $CheckTotal{$check} ) {
	$CheckTotal{$check} += $num;
      }
      else {
	$CheckTotal{$check} = $num;
      }
    }
    $total = $total + $num;
  }
  $title = $test->[0];
  $name  = "TOTAL";
  $number = $total;
  write STDOUT;

  $name = "ONLY GLOBALS";
  ($ocount, $hcount) = getGlobalChecks($test);
  $number = $ocount;
  write STDOUT;

  $name = "ALONE GLOBAL";
  $number = $hcount;
  write STDOUT;

  $name = "ComplCheck";
  $number = getComplicatedChecks($test);
  write STDOUT;
  print STDOUT "\n";

  $name = "totalChecks";
  $number = getFieldSum($test,"totalChecks");
  write STDOUT;
  print STDOUT "\n";

  $name = "totalAssert";
  $number = getFieldSum($test,"totalAssert");
  write STDOUT;
  print STDOUT "\n";

  $name = "octoCheckInsuf";
  if(getFieldSum($test,"totalChecks") > 0) {
  $number = getFieldSum($test,"octoCheckInsuf")/getFieldSum($test,"totalChecks");
  } else {$number = 0;}
  write STDOUT;
  print STDOUT "\n";

  $name = "octoAssertInsuf";
  if(getFieldSum($test,"totalAssert") > 0) {
  $number = getFieldSum($test,"octoAssertInsuf")/getFieldSum($test,"totalAssert");
  } else {$number = 0;}
  write STDOUT;
  print STDOUT "\n";

  $name = "interCheckInsuf";
  if(getFieldSum($test,"totalChecks") > 0) {
  $number = getFieldSum($test,"interCheckInsuf")/getFieldSum($test,"totalChecks");
  } else {$number = 0;}
  write STDOUT;
  print STDOUT "\n";

  $name = "interAssertInsuf";
  if(getFieldSum($test,"totalAssert") > 0) {
  $number = getFieldSum($test,"interAssertInsuf")/getFieldSum($test,"totalAssert");
  } else {$number = 0;}
  write STDOUT;
  print STDOUT "\n";

  if( defined $CheckTotal{CheckTotal} ) {
    $CheckTotal{CheckTotal} += $total;
  }
  else {
    $CheckTotal{CheckTotal} = $total;
  }
}


sub getPhaseAndTime {
  my $line = shift;
  my $p = "nophase";
  my $t = 0.0;

  $line =~ /^\s*(\S*)\s*(\S*)/;
  #print STDOUT "$1 $2\n";
  foreach my $phase (@OptimPhases) {
    if( $1 eq $phase ) {
      $p = $phase;
      $t = $2;
      last;
    }
  }

  return ($p, $t);
}

sub reportOptimPerf {
  my $test = shift;
  my %Times;

  open RESFILE, "<$test->[0].perfsuite.txt"
    or die "Could not open $test->[0].perfsuite.txt";

  while(<RESFILE>) {
    my $line = $_;
    my($phase,$time) = getPhaseAndTime($line);

    if( $phase ne "nophase" ) {
      if( defined $Times{$phase} ) {
	$Times{$phase} += $time;
	$TimesTotal{$phase} += $time;
      }
      else {
	if( defined $TimesTotal{$phase} ) {
	  $TimesTotal{$phase} += $time;
	}
	else {
	  $TimesTotal{$phase} = $time;
	}
	$Times{$phase} = $time;
      }
    }
  }

  close RESFILE;

  foreach my $phase (keys(%Times)) {
    $title = $test->[0];
    $name  = $phase;
    $number = $Times{$phase};
    write STDOUT;
  }
  print STDOUT "\n";
}

sub reportTestTime {
  my $test = shift;
  my $time = 0;

  open RESFILE, "<$test->[0].perfsuite.txt"
    or die "Could not open $test->[0].perfsuite.txt";

  while(<RESFILE>) {
    my $line = $_;
    $line =~ /^\s*(\S*)\s*(\d*)m(\d*.\d*)s/;
    if( defined $1 and $1 eq "user" ) {
      $time = $2 * 60 + $3; # runtime in seconds
    }
  }

  $TestTimes{$test->[0]} = $time;
  $testTimeTotal += $time;

  $title = $test->[0];
  $name  = "test-runtime";
  $number = $time;
  write STDOUT;
  print STDOUT "\n\n";
}

sub reportBaseTime {
  my $test = shift;
  my $time = 0;

  open RESFILE, "<$test->[0]-base.perfsuite.txt"
    or die "Could not open $test-base->[0].perfsuite.txt";

  while(<RESFILE>) {
    my $line = $_;
    $line =~ /^\s*(\S*)\s*(\d*)m(\d*.\d*)s/;
    if( defined $1 and $1 eq "user" ) {
      $time = $2 * 60 + $3; # runtime in seconds
    }
  }

  $BaseTimes{"$test->[0]-base"} = $time;

  $title = $test->[0];
  $name  = "base-runtime";
  $number = $time;
  write STDOUT;
  print STDOUT "\n\n";
}

sub reportOptimTotals {
  my $total_time = $TimesTotal{"TOTAL"};
  foreach my $phase (keys(%TimesTotal)) {
    if( $phase ne "TOTAL" ) {
      $title = "total";
      $name  = $phase;
      $number = $TimesTotal{$phase}/$total_time;
      write STDOUT;
    }
  }
}

sub analyzeResults {
  foreach my $test (@Tests) {
    reportChecks($test);
    reportOptimPerf($test);
    reportTestTime($test);
  }
  reportOptimTotals();
}

sub setBaseline {

  open BASELINE, ">perfsuite.baseline.txt"
    or die "Could not open perfsuite.baseline.txt for writing";

  foreach my $check (keys %CheckTotal) {
    print BASELINE "$check $CheckTotal{$check}\n";
  }

  foreach my $phase (keys %TimesTotal) {
    print BASELINE "$phase $TimesTotal{$phase}\n";
  }

  foreach my $test (keys %TestTimes) {
    print BASELINE "$test $TestTimes{$test}\n";
  }

  foreach my $test (keys %BaseTimes) {
    print BASELINE "$test $BaseTimes{$test}\n";
  }

  print BASELINE "testTimeTotal $testTimeTotal\n";

  close BASELINE;

}

sub readBaseline {
  my %Baseline;

  open BASELINE, "<perfsuite.baseline.txt"
    or die "Could not open perfsuite.baseline.txt";

  while(<BASELINE>) {
    /^\s*(\S*)\s*(\S*)/;
    $Baseline{$1} = $2;
  }

  close BASELINE;

  return %Baseline;
}

sub compareWithBaseline {
  my %Baseline = readBaseline();

  print STDOUT "Comparing with baseline\n";
  foreach my $check (keys %CheckTotal) {
    my $new = $CheckTotal{$check};
    my $old = $Baseline{$check};

    $title = "percent diff";
    $name  = $check;
    if( not defined $old ) {
      $number = 0;
    }
    elsif( $old == 0 ) {
      $number = 0;
    }
    else {
      my $diff = $new - $old;
      print STDOUT "\n$check: new count: $new, old count: $old, difference: $diff\n";
      $number = ($new - $old)/$old;
    }
    write STDOUT;
  }
  print STDOUT "\n";

  my $optimCost = 0;
  foreach my $phase (keys %TimesTotal) {
    my $new = $TimesTotal{$phase};
    my $old = $Baseline{$phase};

    $title = "";
    $name  = $phase;
    if( not defined $old ) {
      $number = 0;
    }
    elsif( abs($new - $old) < .01 ||
	   $old == 0 ) {
      $number = 0;
    }
    else {
      $number = ($new - $old)/$old;
    }
    write STDOUT;
    if($phase ne "optimizer-stats:TOTAL") {
      $optimCost = $optimCost + $new;
    }
  }
  print STDOUT "\n";

  $title = "";
  $name = "% of Deputy";
  $number = $optimCost / $TimesTotal{"TOTAL"};
  write STDOUT;

  my $totchange = 0;
  my $numtests = 0;
  foreach my $test (keys %TestTimes) {
    my $new = $TestTimes{$test};
    my $old = $Baseline{$test};
    
    $title = $test;
    $name = "%run-time d";
    if( not defined $old ) {
      $number = 0;
    }
    elsif( abs($new - $old) < .01 || $old == 0 ) {
      $number = 0;
    }
    else {
      $number = ($new - $old) / $old;
    }
    write STDOUT;
    
    $totchange += $number;
    $numtests++;
  }

  $title = "";
  $name = "avg change";
  $number = $totchange / $numtests;
  write STDOUT;

  $totchange = 0;
  $numtests = 0;
  foreach my $test (keys %TestTimes) {
    my $new = $TestTimes{$test};
    my $old = $Baseline{"$test-base"};
    
    $title = $test;
    $name = "%run-time d";
    if( not defined $old ) {
      $number = 0;
    }
    elsif( abs($new - $old) < .01 || $old == 0 ) {
      $number = 0;
    }
    else {
      $number = ($new - $old) / $old;
    }
    write STDOUT;
    
    $totchange += $number;
    $numtests++;
  }
  
  $title = "";
  $name = "avg slowdown";
  $number = $totchange / $numtests;
  write STDOUT;

  my $baseTestTime = $Baseline{"testTimeTotal"};
  $title = "";
  $name = "run-time";
  if( abs($testTimeTotal - $baseTestTime) < .01 ) {
    $number = 0;
  }
  else {
    print STDOUT "new: $testTimeTotal base: $baseTestTime\n";
    $number = ($testTimeTotal - $baseTestTime)/$baseTestTime;
  }
  write STDOUT;
}

sub handleArgs {
  my $next_is_opt = 0;

  foreach my $arg (@ARGV) {
    if( $next_is_opt == 1) {
      $opt = $arg;
      $next_is_opt = 0;
    }
    elsif( $arg eq "-h" or
	   $arg eq "--help" ) {
      $help = 1;
    }
    elsif( $arg eq "-r" or
	   $arg eq "--runtests" ) {
      $rt = 1;
    }
    elsif( $arg eq "-B" or
	   $arg eq "--baseline" ) {
      $base = 1;
    }
    elsif( $arg eq "-o" ) {
      $next_is_opt = 1;
    }
    elsif( $arg eq "-p" or
           $arg eq "--patch") {
      $popt = "PATCH=1";
    }
  }
}

sub printUsage {
  print STDOUT "perfsuite.pl [options]\n";
  print STDOUT "\t-h,--help     : print this message\n";
  print STDOUT "\t-r,--runtests : rerun the tests\n";
  print STDOUT "\t-B,--baseline : set the results of this run as the baseline\n";
  print STDOUT "\t-o num        : set the optimization level\n";
  print STDOUT "\t-p, --patch   : include patch files\n";
}

sub main {

  handleArgs();

  if( $help ) {
    printUsage();
    exit;
  }

  mkdir "perftest" if not -e "perftest";

  runTests() if $rt == 1;
  analyzeResults();
  compareWithBaseline() if $base == 0;
  if( $base == 1 ) {
    runBaseTests();
    foreach my $t (@Tests) {
      reportBaseTime($t);
    }
    setBaseline();
  }
}



main();
