This directory contains example outputs produced by the programs in the
directory above.

Each image here is a graph. Plotted along the X-axis are Dr. Mario pill
sequences. The Y axis is a representation of how much more (or less) likely
that sequence is to appear when playing the NES version of Dr. Mario than it
would be if pills were actually chosen uniformly at random. Specifically, it is
the ratio between the probability of seeing that sequence on the NES and the
probability of seeing that sequence in IID uniform selections. Amounts over 1
indicate that the NES generates that sequence too much; under 1 indicates the
NES generates that sequence too infrequently. The Y axis is on a log scale, so
that the bar for a sequence that is twice as likely as it ought to be is the
same size as a bar for a sequence that is half as likely as it ought to be.

The X axis is labeled by pictures describing the sequence. Pill sequences are
shown with time flowing from top to bottom, so that the top pill would be the
first one a player would have to choose a position for, and the bottom pill is
the last one a player would have to choose a position for.

This directory contains graphs for all sequences of length 1, 2, and 3. It also
contains two variants of graph for each sequence length. Strategically, it
almost always means exactly the same thing to get a blue-yellow pill as it does
to get a yellow-blue pill. So one variant groups together sequences that are
equivalent under such mirrorings; the other doesn't. These two variants are
called "exact" -- for when sequences are only considered equal if the pills are
literally the same -- and "quotient" -- for when sequences are considered equal
if there is some collections of rotations that converts one sequence to the
other. The graphs also visually indicate this distinction: pills rendered
horizontally indicate the current graph was built with an exact equality, while
vertical pills indicate the current graph was built with the quotiented
equality.

There is also a file that contains all of the pill sequences used, in case
you'd like to do your own analyses. It is a bzipped text file. In the file, you
will see many copies of this sequence:
* a seed (a 16-bit number) on its own line,
* 128 pills, each on their own line, with the color (y, r, or b) of the left
  half first, then the right half, and
* a blank line separator.
There are 32767 chunks in this format. (The NES does not use seed 0; it uses an
LFSR, and if an LFSR ever hits 0 it stays there forever, which doesn't look
very random to the player. Odd seeds produce the exact same pill sequences as
their next-door neighbor one down.)
