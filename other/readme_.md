[quilt](https://github.com/tonyday567/quilt) [![Build Status](https://travis-ci.org/tonyday567/quilt.svg)](https://travis-ci.org/tonyday567/quilt)
================================================================================================================================================================================

This is where and how I go about a haskell library refactor. I pick a new ghc version, and a new stack lts, and set this project up with no compile warts.  Each addition adds a constellation of libraries I use, and a logic compilation milepegs.

[readme-lhs](https://github.com/tonyday567/readme-lhs)
---

This is a thin wrap around protolude and pandoc, containing much loved functionality. This sets Text as a major primitive type.

``` {.output .example}
```

[numhask-space](https://github.com/tonyday567/numhask-space)
---

Some concrete geometry.

``` {.output .NumHask.Space}
```

[numhask-array](https://github.com/tonyday567/numhask-array)
---

n-dimensional arrays

``` {.output .NumHask.Array}
```

[box](https://github.com/tonyday567/box)
---

A profunctor with STM queues at each end.

``` {.output .Box}
```

[web-rep](https://github.com/tonyday567/web-rep)
---

Representations of web pages.

``` {.output .web-rep}
```

[chart-svg](https://github.com/tonyday567/chart-svg)
---

Numerical charts in svg.

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

[online-market](https://github.com/tonyday567/online-market/blob/master/runs/default/index.html)
---


workflow
---

    stack build --test --exec "$(stack path --local-install-root)/bin/quilt" --file-watch
