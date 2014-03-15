Regular Expression Derivatives in Rust
--------------------------------------

Regex derivatives due to Brzozowski.  Super neat!

So first version is a port of Matt Might's scheme to Rust. This takes the
derivatives as it matches.  Doesn't include the boolean operators.

Surprisingly to me, it's shorter than the Scheme version.

http://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/

Now, most people don't want to type in what is essentially the AST of the regex
so let's write a little recursive descent parser.  Once again I'll refer you to
Matt Might's blog for more information:

http://matt.might.net/articles/parsing-regex-with-recursive-descent/

Next up is one of the coolest parts of the whole thing:
We can construct a DFA equivalent to the regex using derivatives.  This comes
from the original Brzozowski paper I think, but I haven't read it.

The canonical "tutorial" on this work is due to Owens, Reppy, Turon:
http://www.mpi-sws.org/~turon/re-deriv.pdf

So basically, you have a DFA.  Each node is labelled with a regex.
Your start node is labelled with the original, and then for each possible
input character, take the regex derivative.  Make a transition for each of
those characters to the new nodes -- likely the number of edges is much smaller
than the number of characters though.  And iterating over the whole alphabet is
expensive, so we can do this a bit smarter.

There's only probably a few different derivatives of each regex, so we can make
a function that partitions the input space into few equivalence classes, and
then only need to compute derivatives once per class.

We still need to compare regexes to all the nodes in the graph to avoid making
a ton of states, and comparing regexes is tricksy.  So we only approximately
compare them, which gets us pretty close.

We can run a DFA minimizer after, too, though the approximations above are
probably good enough.

We can match regexes on the DFA once constructed!

Since we can basically match regexes at any stage of the pipeline, we can fuzz
the whole thing by generating regexps and random strings, ensuring they all
consistently match.
