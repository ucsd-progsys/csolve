
# This file will be included from deputy/test/testdeputy.pl
sub addExt {
    my($command, %args) = @_;
    my $tst = addDeputyTest($command, %args);
    # Directory relative to the deputy/test directory
    $tst->{Dir} = "../..";
}



## OLDEN

foreach my $o ("bh", "bisort", "em3d", "health", "mst", "perimeter",
               "power", "treeadd", "tsp") {
    addExt("$o", Group => ['slow']);
    addExt("$o-base", Group => ['slow']);
}

## PTRDIST
foreach my $p ("anagram", "bc", "ks", "ft", "yacr2") {
    addExt("$p", Group => ['slow']);
    addExt("$p-base", Group => ['slow']);
}

## SPEC95
foreach my $p ("go", "li") {
    addExt("$p", Group => ['slow']);
    addExt("$p-base", Group => ['slow']);
}

## SPEC00
foreach my $p ("gzip") {
    addExt("$p", Group => ['slow']);
    addExt("$p-base", Group => ['slow']);
}

## mediabench
foreach my $p ("adpcm", "epic") {
    addExt("$p", Group => ['slow']);
    addExt("$p-base", Group => ['slow']);
}

# Add performance tests
sub addPerf {
    my($name, %args) = @_;
    my $tst = addDeputyTest("perftest/$name-base NODEPUTY=1", %args);
    # Directory relative to the deputy/test directory
    $tst->{Dir} = "../..";

    my $tst = addDeputyTest("perftest/$name", %args);
    # Directory relative to the deputy/test directory
    $tst->{Dir} = "../..";

}


foreach my $t ("array1", "array2", "array3", 
               "array_incr", "string1", 
               "matxmult", "matxmult2") {
    addPerf($t, Group => ['slow', 'perf']);
}



# Make the host script happy
1; 
