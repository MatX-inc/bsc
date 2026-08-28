#!/usr/bin/perl
# Filter the test list (one .exp path per line, as produced by
# sort-by-time.pl) down to the directories named in $TESTDIRS, a
# space-separated list of directory prefixes relative to the testsuite
# root (e.g. "bsc.mcd bsc.lib/BRAM").  Matching is directory-boundary
# aware: "bsc.lib" matches bsc.lib/... but not bsc.libextra/...
#
# A pass-through no-op when TESTDIRS is unset or empty, so local runs
# are unaffected.  Used by CI to shard the testsuite across parallel
# jobs; the empty-list guard in suitemake.mk makes a TESTDIRS value
# that matches nothing loudly fatal rather than a silent zero-test run.
use strict;
use warnings;

my $dirs = $ENV{TESTDIRS};
if (!defined($dirs) || $dirs =~ /^\s*$/) {
    print while (<STDIN>);
    exit 0;
}

my @prefixes;
foreach my $p (split(' ', $dirs)) {
    $p =~ s{/+$}{};
    push @prefixes, $p if length $p;
}

while (my $line = <STDIN>) {
    my $path = $line;
    $path =~ s/^\s+//;
    $path =~ s/\s+$//;
    $path =~ s{^\./}{};
    foreach my $p (@prefixes) {
        if ($path eq $p || index($path, "$p/") == 0) {
            print $line;
            last;
        }
    }
}
