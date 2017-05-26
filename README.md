A very simple, backtracking, fast parser combinator library.

It is measurably faster than [attoparsec](https://hackage.haskell.org/package/attoparsec) (36% in [this use case](https://hbtvl.banquise.net/posts/2015-12-14-fastParsing03.html)), but only works on strict `ByteString`, lacks many helper functions, and is not resumable.
It also should consume a tiny bit less memory for equivalent operations.

# When NOT to use fastparser

 * When performance is not the **most** pressing concern.
 * When you need to parse anything else but strict `ByteString`.
 * When you need to use a battle-tested library. While very simple, and in constant use by me, this package is still quite experimental.
 * When you need to parse large inputs that are not easily cut into many smaller pieces that can be parsed independently.

# How to use fastparser

`fastparser` works well with small pieces, such as individual log file lines. It is recommended to use it with a coroutine library (such as [conduit](http://hackage.haskell.org/package/conduit) or [pipe](http://hackage.haskell.org/package/pipes)), so that the input could be incrementaly consumed, cut into individual records, all of which would end up parsed independently.

One such setup, with the `conduit` ecosystem, would look like:

    sourceFile "/tmp/foo" .| Data.Conduit.Binary.lines .| CL.map (parseOnly parser) .| ...

Other than that, `fastparser` is fairly close to any other parser combinators library.
