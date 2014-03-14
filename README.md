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
