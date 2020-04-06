[quilt](https://github.com/tonyday567/quilt) [![Build Status](https://travis-ci.org/tonyday567/quilt.svg)](https://travis-ci.org/tonyday567/quilt)
===

> True greatness is measured by how much freedom you give to others, not by how much you can coerce others to do what you want. ~ Larry Wall

This is a patchwork quilt of all my favorite libraries I like to keep up to date with ghc.

[numhask-prelude](https://hackage.haskell.org/package/numhask-prelude)
---

There are many preludes, but this one is mine.

Once I have my chosen build for this, I tend to do the global installs, and especially the hie friendly step `stack hoogle --keep-going` along the way, as I add the rest of the libraries.

[readme-lhs](https://github.com/tonyday567/readme-lhs)
---

readme-lhs is a wrapper around pandoc that gives me a one-way write channel into a markdown file. 

Add code blocks to markdown files

```
    ``` {.output .example}
    ```
```

then use the runOutput monad

```{.haskell}
void $ runOutput ("other/readme_.md", GitHubMarkdown) ("readme.md", GitHubMarkdown) $ do
    output "example" (Fence "Simple example of an output")

```

and output is inserted in the markdown:

``` {.output .example}
```

You can then put a stack loop on like so:

```
stack build --test --exec "$(stack path --local-install-root)/bin/quilt" --file-watch
```

and nice markdown output is right there for upstream processes like blogging and logging.

If you code in haskell you really should use the native [pandoc](https://hackage.haskell.org/package/pandoc) api for text-based output.

[numhask-space](https://github.com/tonyday567/numhask-space)
---

Provides all sorts of spaces, ranges and grids. The api is general over a wide range of numbers including times, which can be tricky with boundary finding. An example; the next 500 days, marked with sensible date milestones:

``` {.output .NumHask.Space}
```

[numhask-array](https://github.com/tonyday567/numhask-array)
---

numhask-array is an n-dimensional array library I'm extremely proud of. Matrix multiplication is expressed like so:

```
let b :: Array '[2, 3] Double = fromList [1 .. 6]
dot sum (*) b (F.transpose b)
```

``` {.output .NumHask.Array}
```

It is rare to see matrix multiplication abstracted in this way, with an exposed binary operator and then an exposed fold operation. APL comes to mind but none of the numeric inclined fashions.

And when, in haskell, you can make something like this polymorphic, ghc can work miracles behind the scenes. This next example looks for vector matches within a matrix

```
-- >>> let cs = fromList ("abacbaab" :: [Char]) :: Array '[4,2] Char
-- >>> let v = fromList ("ab" :: [Char]) :: Vector 2 Char
-- >>> dot (all id) (==) cs v
-- [True, False, False, True]
```

[box](https://github.com/tonyday567/box)
---

A profunctor with queues at each end.

``` {.output .Box}
```

[web-rep](https://github.com/tonyday567/web-rep)
---

This brings in lens, javascript, clay, lucid and scotty, and provides representations of web pages.

``` {.output .web-rep}
```

[chart-svg](https://github.com/tonyday567/chart-svg)
---

Numerical charts targetting svg as the backend.

``` {.output .chart-svg}
```

[perf](https://github.com/tonyday567/perf)
---

low-level performance stats

``` {.output .perf}
```

Inner array loop for numhask-array.

``` {.output .inner}
```

[online](https://github.com/tonyday567/online)
---

rolling statistics

``` {.output .online}
```

My active projects.

[online-market](https://github.com/tonyday567/online-market)
---

[online-covid](https://github.com/tonyday567/online-covid)
---

global installs
---

- hoogle
- hie
- hie-wrapper
- hlint
- weeder
- ormolu
- ghcid
- haddock
- pandoc

workflow
---

    stack build --test --exec "$(stack path --local-install-root)/bin/quilt" --file-watch
