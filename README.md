# funlangs
Code samples for Functional Languages

* Week 8

  - `Relabel.lhs`: Example programs implementing a tree-relabeling
    process in a purely functional style, including both a direct
    version and an alternative that uses a monad to hide some of
    the underlying plumbing.  See comments in file for changes to
    make if you want to use this file with GHC.

  - `RelabelTrans.lhs`: An extension of `Relabel.lhs` that uses
    monad transformers to build a suitable monad instead of a
    custom-built monad.

  - `State.lhs`: A simple state monad library.

* Week 7

  - `game.lhs`: The simple guessing game developed in class at
    the end of the lecture on Feb 19.

  - `mailmerge/`: A small demo using a combination of IO actions
    (with `do` notation) and a purely functional transformation
    on `String`s to implement a simple mail merge program.

  - `Set.lhs`: An implementation of sets as characteristic
    functions.

  - `artwork/*`: A library for constructing images as function
    values.

  - `ParserCombs.lhs`: A library of "parser combinators", using
    function values to represent and implement a broad range of
    parsing mechanisms.  The code in this file is intended for
    use with Hugs.

  - `ParserCombsGHC.lhs`: A version of the `ParserCombs.lhs`
    library that is suitable for use with GHC/GHCi.  (The only
    real difference is that the GHC version includes the required
    instance for the `Applicative` class.)

* Week 6

  - `IOActions.lhs`: A repackaging of some standard Haskell
    functions to be used "IO Actions".

  - `IOExercises.lhs`: A template for some exercises using
    IO Actions.

  - `WebActions.lhs`: A library of IO actions for downloading
    files and processing web pages.  (See `IOActions-README.txt`
    for further details about these files, including information
    on how to install the necessary packages to use
    `WebActions.lhs`.)

  - `grading.lhs`: A demo program illustrating how we
    can use Haskell as a basic calculator.

* Week 5

  - `trees.hs`: Definitions for several different types of
    tree data structure, and for a type class that captures
    common properties between them, allowing us to write
    generic functions that work on a range of different tree
    types.

  - `pathological.lhs`: An example illustrating pathological
    behavior of Haskell-style type inference.

  - `subtleties.lhs`: Some examples illustrating some subtle
    details of Haskell-style type inference.

  - `Treedot.lhs`: A library for generating GraphViz/dot files
    for tree-like data structures using a simple variant of the
    approach that was presented in lectures.

* Week 4

  - `countdown.lhs`:  Code for solving the countdown problem,
    developed in class, but heavily based on the implementation
    in Chapter 9, "Programming in Haskell (2nd ed)" by Graham
    Hutton.

  - `aug.hs`: Code for "Using Types to Parse Natural Language".
    The paper and the original version of the code is available
    at http://web.cecs.pdx.edu/~mpj/pubs/aug.html.

* Week 3

  - `datatypes.lhs`: Some examples of datatype definitions
    from the lecture.

  - `bst.lhs`: A literate version of the binary search tree
    example from the lecture.

  - `Pic.lhs`: A library for drawing text "pictures".

  - `TreeEdit.lhs`: An interactive tree editor.

* Week 2

  - `group.lhs`: Some examples demonstrating the use of
    function composition to build some interesting utility
    functions, as well as an example illustrating the use
    of literate files to include commentary and sample
    outputs.

  - `say.lhs`: A little program for printing short messages
    using big characters (formed out of little characters)!

  - `fibs.lhs`: Various ways to implement Fibonacci functions,
    (and to test that they produce the same results ...)

  - `pascal.lhs`: A demonstration program that can display
    versions of Pascal's triangle, built using standard list
    processing functions.

* Week 1

  - `defs.hs`: an example showing some simple definitions

  - `composing-fractals.lhs`: a single file version of the
    fractals program

  - `fractals/`: a folder containing a more sophisticated
    version of the fractals program, spread over multiple
    "modules", including "literate" code, and using the
    GHC compiler.


