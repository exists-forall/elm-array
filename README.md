# elm-array

This project is an attempt to reimplement the logic of [Elm's core `Array` module](http://package.elm-lang.org/packages/elm-lang/core/latest/Array) in Elm.  The existing standard library `Array` module is implemented in Javascript, and has [known bugs](https://github.com/elm-lang/core/issues?utf8=✓&q=is%3Aissue+is%3Aopen+arrays).  This project is intended to fix the existing bugs in `Array`, while simualtaneously creating a more maintainable foundation for future work (thanks to the benefits of functional purity and static typing).  It is very much a work in progress.

## The data structure

The `Array` type is backed by a data structure called a ["Relaxed Radix Balanced Tree"](http://infoscience.epfl.ch/record/169879/files/RMTrees.pdf), or RRB-Tree.  RRB-Trees support logarithmic-time indexing, updating, appending, and concatenation, while preserving immutability.  Online information on RRB-Trees is scarce, and the original paper that describes them is short and often unclear.  The following appear to be some of the best supplemental resources:

- [A "prototype" implementation in Scala](https://github.com/TiarkRompf/rrbtrees/blob/master/RRBVector.scala) (by the authors of the original paper)
- [This Master's Thesis, which elaborates on the ideas of the original paper](http://hypirion.com/thesis.pdf) (different author than the original paper)
- [This C implementation](https://github.com/hyPiRion/c-rrb) (by the author of the Master's thesis)
- [The implementation in the Clojure standard library](https://github.com/clojure/core.rrb-vector) (possibly by the author of the Master's Thesis?)

## Design

This `Array` implementation is designed to be, at minimum, a drop-in replacement for the existing `Array` module in the standard library, with exactly the same API.

This implementation is not and cannot be written entirely in Elm.  RRB-Trees internally make use of fixed-size, copy-on-modify, random-access, memory-contiguous arrays (with a lowercase "a"), which do not exist in the Elm standard library.   Consequently, these simple arrays (called `Table`s in the codebase to avoid confusion with `Array`s) must be implemented in a small Javascript module.  The complex logic of the RRB-Tree itself is implemented in Elm.

## Current status

This repository currently includes a 100% API-compatible reimplementation of `Array`'s logic in Elm.  It is not yet ready for production use, but it captures all the essential features of the RRB-Tree.  With further testing, profiling, and cleanup, it will hopefully be usable as a better-than-ever replacement for `Array` in the not-too-distant future.

~~To help the "exploratory" phase of the project, a temporary and inefficient `NaiveTable` module was introduced to stand in for the eventual native-Javascript `Table` module.  `NaiveTable` is written in Elm and makes use of linked lists to emulate true random-access arrays, giving the same semantics as a true `Table`, but with vastly worse performance.  Now that its API is mostly stabilized, `NaiveTable` will soon be replaced with a performant native-Javascript version.~~

## Roadmap

### Well-defined tasks:

- [X] Implement every function in the existing `Array` API
- [X] Reimplement `NaiveTable` in Javascript
- [ ] Address every `TODO` comment in the codebase
- [X] Add justifications for every `assumeJust` call in the codebase, for use as both comments and potential error messages
- [ ] For debug purposes: write a function to test if tree conforms to invariants, routinely call this function after every tree operation, and raise an error / log a warning if an invariant is broken.

### Larger, open-ended tasks:

- [ ] Test thoroughly, using at minimum the existing standard library unit tests
- [ ] Profile thoroughly, especially in comparison to...
    - [ ] The existing `Array` module (under conditions where it actually works)
    - [ ] Simpler, theoretically-less-performant data structure like `List`s and `Table`s, especially for small, common use cases
    - [ ] Simpler tree-based persistent array structures
- [ ] General code cleanup
- [ ] Community code review
