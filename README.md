# advent-of-code-2023

Advent of Code 2023 in CHICKEN Scheme (until I can't do it anymore and fallback to something nicer like C++)!

## Lessons from each day

- 01: Recursion makes my brain hurty
- 02: `defstruct` is a life saver and slowly learning how to use the standard libs
- 03: hash tables are in `(srfi 69)`, and `(chicken plist)` is for `(symbol value)` list pairs. Also `printf` is cool
- 04: Vectors are fixed length lists with $O(1)$ read write operations
- 05: Life would be easier if you always treat ranges as `(start, end)` and not `(start, length)`. Also why loop when you can `(map)`!
- 06: `(floor)` and `(ceiling)` also do float to integer parsing, but then need to recover exactness with `(inexact->exact)`. Also `(map)` accepts as many lists as function airity!
- 07: `(plist)` is useful but the way you have to pass the symbol around instead of e.g. the list itself is counter-intuitive. Also I can't work out how to do `if any` on a list?
- 08: `(chicken irregex)` are IrRegular Expressions, that are also constructed from s-expressions.
- 09: `(define ((curry f x) y) (f x y))` is *chef's kiss*. Also SRFI-1 has an `every` that does what I wanted with `all/any`!
- 10: it might be better to use data structures instead of remembering if it's `car` or `cdr` next. [Pick's Theorem](https://en.wikipedia.org/wiki/Pick%27s_theorem) is very cool!
- 11: i didn't really learn anything new from this one
- 12: memoization is easy but you need the egg. i can't work out a nice tail-call recursive pattern for solution though, since i need to (maybe) add the result to a variable (twice) and then return that
