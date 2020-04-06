[quilt](https://github.com/tonyday567/quilt) [![Build Status](https://travis-ci.org/tonyday567/quilt.svg)](https://travis-ci.org/tonyday567/quilt)
==================================================================================================================================================

The above link is a haskell library that I am using to upgrade to
lts-15.6 and ghc-8.8.3. If the light is green, then I'm where I want to
be.

> True greatness is measured by how much freedom you give to others, not
> by how much you can coerce others to do what you want. \~ Larry Wall

[numhask-prelude](https://hackage.haskell.org/package/numhask-prelude)
----------------------------------------------------------------------

This is where I start. `numhask-prelude` brings in core libraries, and
sets up numeric and Text channels.

This is also what you get with the
[readme-lhs](https://github.com/tonyday567/readme-lhs/blob/master/other/readme-lhs.hsfiles)
stack template.

[readme-lhs](https://github.com/tonyday567/readme-lhs)
------------------------------------------------------

readme-lhs is a wrapper around pandoc that gives me a one-way write
channel into a markdown file.

-   Add code blocks to markdown files that looks something like this:

          ``` {.output .example}
          ```

-   Use Readme.Lhs.runOutput to insert output, like this:

``` {.haskell}
void $ runOutput ("other/readme_.md", GitHubMarkdown) ("readme.md", GitHubMarkdown) $ do
    output "example" (Fence "Simple example of an output")
```

-   And produce this:

``` {.output .example}
Simple example of an output
```

-   Put this in a stack loop like this:

<!-- -->

    stack build --test --exec "$(stack path --local-install-root)/bin/quilt" --file-watch

-   Pipe the markdown file somewhere to render it, and you have a very
    tight workflow.

-   Use Pandoc Native

    -   If you code in haskell use the native
        [pandoc](https://hackage.haskell.org/package/pandoc) api for
        output, and if you can't convert, it shouldn't exist.

[numhask-space](https://github.com/tonyday567/numhask-space)
------------------------------------------------------------

Provides all sorts of spaces, ranges and grids. The api is general over
a wide range of numbers including times, which can be tricky with
boundary finding. The next 500 days, marked with sensible date
milestones:

![](other/timespace.svg)

[numhask-array](https://github.com/tonyday567/numhask-array)
------------------------------------------------------------

numhask-array is an n-dimensional array library I'm extremely proud of.
Matrix multiplication is expressed like so:

    let b :: Array '[2, 3] Double = fromList [1 .. 6]
    dot sum (*) b (F.transpose b)

``` {.output .NumHask.Array}
[[14.0, 32.0],
 [32.0, 77.0]]
```

It is rare to see matrix multiplication abstracted in this way, with an
exposed binary operator and then an exposed fold operation. APL comes to
mind but none of the numeric inclined fashions.

And when, in haskell, you can make something like this polymorphic, ghc
can work miracles behind the scenes. This next example looks for vector
matches within a matrix

    -- >>> let cs = fromList ("abacbaab" :: [Char]) :: Array '[4,2] Char
    -- >>> let v = fromList ("ab" :: [Char]) :: Vector 2 Char
    -- >>> dot (all id) (==) cs v
    -- [True, False, False, True]

[box](https://github.com/tonyday567/box)
----------------------------------------

A profunctor with queues at each end.

echo: hi

echo: bye

[web-rep](https://github.com/tonyday567/web-rep)
------------------------------------------------

This brings in lens, javascript, clay, lucid and scotty, and provides
representations of web pages.

``` {.output .web-rep}
<!DOCTYPE HTML><html lang="en"><head><meta charset="utf-8"></head><body><script>window.onload=function(){}</script></body></html>
```

[chart-svg](https://github.com/tonyday567/chart-svg)
----------------------------------------------------

Numerical charts targetting svg as the backend.

![](other/chart-svg.svg)

global installs
---------------

In order:

-   hoogle
-   haddock
-   weeder
-   hlint
-   ormolu
-   hie
-   hie-wrapper
-   ghcid
-   pandoc

GUI
---

With a pristine install, hie works out of the box with spacemacs.

    (haskell
          :variables
          haskell-completion-backend 'lsp
          haskell-process-suggest-remove-import-lines nil
          lsp-haskell-process-path-hie "hie-wrapper"
          )

This seems to help everything along:

    stack haddock --keep-going

[perf](https://github.com/tonyday567/perf)
------------------------------------------

low-level performance stats

![](other/perf.svg)

[online](https://github.com/tonyday567/online)
----------------------------------------------

rolling statistics

![](other/online.svg)

My active projects.
===================

[online-market](https://github.com/tonyday567/online-market)
------------------------------------------------------------

[online-covid](https://github.com/tonyday567/online-covid)
----------------------------------------------------------

workflow
--------

    stack build --test --exec "$(stack path --local-install-root)/bin/quilt" --file-watch

I build up in order, and, the resulting patchwork is:

    resolver: lts-15.6

    packages:
      - .

    extra-deps:
      - backprop-0.2.6.3
      - box-0.2.0
      - chart-svg-0.0.1
      - interpolatedstring-perl6-1.0.2
      - javascript-bridge-0.2.0
      - lucid-svg-0.7.1
      - numhask-array-0.5.1
      - numhask-prelude-0.3.3
      - numhask-space-0.3.1
      - online-0.4.0.0
      - palette-0.3.0.2
      - perf-0.5.0.0
      - perf-analysis-0.2.0.0
      - readme-lhs-0.5.0
      - tdigest-0.2.1
      - text-format-0.3.2
      - web-rep-0.3.1
      - git: https://github.com/tonyday567/online-market.git
        commit: 5e3571de84cdf31d3acc8f7eff7a527e04baf47a
      - monad-bayes-0.1.0.0@sha256:7ac7ef909cdf8247754e557172e8fd1047c413bdb9dca5153a3fbe33676112b8
      - vinyl-0.12.1@sha256:03f5e246fae2434250987bbfe708015dc6e23f60c20739c34738acde1383b96c
      - git: https://github.com/tonyday567/pmlb
        commit: 5b1d7fe0a6ed6451aea844f74d543e67cdc2eb11
      - streaming-utils-0.2.0.0@sha256:a2bd9144b336393122ffed2d3a1c747c186b8fdb9734c54d8b5874ed6166a85b
      - json-stream-0.4.2.4@sha256:8b7f17d54a6e1e6311756270f8bcf51e91bab4300945400de66118470dcf51b9
