                   SPEC Benchmark Specification
                                 Go

1.0: General

1.1: Classification

Go is a cpu-bound integer benchmark.  It is an example of the use of
artificial intelligence in game playing.

1.2: Description

Go plays the game of go against itself.  The benchmark is stripped
down version of a successful go-playing computer program.

The benchmark is implemented in ANSI C (with function prototypes).

There is a great deal of pattern matching and look-ahead logic.
As is common in this type of program, up to a third of the run-time
can be spent in the data-management routines.


1.3: Source/Author

	David Fotland
	San Jose, CA.

A full functioned verison of this program, Many Faces of Go, with a user
interface, is available for the IBM-PC from Ishi Press, 76 Bonaventura Ave,
San Jose CA 95134, (408) 944-9900, and for PenPoint from PenGames, 4863
Capistrano Ave San Jose, CA 95129 (408)985-1236.

1.4: Version Information

This is a special version of the Go program, The Many Faces of Go, developed
for use as a part of the SPEC benchmark suite.  This go playing engine is
from the 1989 version of Many Faces of Go.  The latest version is a much
stronger go player.

2.0: Performance

2.1: Metric

No special performance measures are produced by Go.  The elapsed 
time to play a game against itself is the measure of performance.  

2.2: Reference Time

TBD.

Approximate times (without any special compiler option tuning):

486-25 PC       about 40 minutes
HP9000/755       2 minutes 51 seconds
HP9000/750       4 minutes 50 seconds
HP9000/400      43 minutes 35 seconds

2.3: Reports

Go writes to stdout a move-by-move listing of the game as it is played.
Error messages will be genereated as appropriate.

3.0: Software

3.1: Language

ANSI C with fucntion prototypes.

3.2: Operating System

Both MS-DOS and UNIX implementations are supported in the source code.  

3.3: Portability

It is strictly ANSI C compliant, and has no dependencies on endianness.

It should be quite portable since it doesn't depend on any unusual runtime
library routines, or on endianness.  The only time it has run differently
on different processors was due to floating point rounding differences in
the initialization of rtval1[] in initrtval() in g23.c.  Modify the
initialization slightly, and this should not be a problem any more, but if
a port to a non-IEEE FP machine gives a different result, look here first.

g2jlib2.c contains a very large initializer, which might give some compilers
problems.

3.4: Others

No other software considerations for Go.  Go does not malloc any memory.

4.0: Hardware

4.1: Memory

TBD.

4.2: Other

No other hardware requirements exist for Go.

5.0: Operational

5.1: Disk Space

No disk requirements beyond the space required to hold the program 
source, executable image, input files and output files.  This space 
is less than a megabyte.

5.2: Installation

The directory contains the source for the Go program and a makefile that
builds it.  The makefile by default uses the host's default C compiler and
specifies optimization with the -O flag.  Compiler options may be passed in
by EXTRA_CFLAGS.

5.3: Execution

To run Go, type:

	time go > go.out

Verify whether the output is correct by:

	diff go.out specout.go

there should be no differences reported by the diff program.

