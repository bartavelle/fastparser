A very simple, backtracking, fast parser combinator library.

It is measurably faster than [attoparsec] (36% in [this use case](https://hbtvl.banquise.net/posts/2015-12-14-fastParsing03.html)), but only works on strict `ByteString`, lacks many helper functions, and is not resumable.
It also should consume a tiny bit less memory.
