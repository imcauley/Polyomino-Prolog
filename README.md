# Prolog Polynomino Solver
A program for solving a polynomino tiling problem

## Running and Input
To run open *SWI* Prolog and load file solver.pl

Open file and run solver with:

`?- main('filename.txt'). `

## Input Files
The input file but be in the following format:

~~~~
Rows Columns
NumPieces
NumBlocks Block1 Block2
NumBlocks Block3 Block4 Block5
etc
~~~~

Example:

~~~~
2 4
2
4  0 0  1 0  0 1  1 1
4  0 0  1 0  0 1  1 1
~~~~

For more examples see the input folder.
