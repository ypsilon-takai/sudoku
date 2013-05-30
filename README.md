# SUDOKU

Sudoku solver written in clojure.

The trigger to write this is to solve project euler problem 96 and now it is fullfilled, but I will add interface to solve another problem in the future.

## Getting the answer of pe86 problem.

Run REPL, into ns 'sudoku.pe86' and run (pe86)

## Strategy

I programed three rules which I used to use to solve Sudoku. And also programed backtracker for the problem which wouldn't be solved with these rules.

### Rules

- Only one candidate.
- Not in neibor.
- Two number in only two cells.

## License

Copyright 2013 Ypsilon.Takai

Distributed under the Eclipse Public License, the same as Clojure.
