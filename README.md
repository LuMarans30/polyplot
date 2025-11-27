# Polyplot

Interactive 3D polyhedron visualizer

## Usage

- `cabal build`
- `./polyplot <filename>`

## Input format

A text file where each line defines a half-space:
```
a1 a2 a3 b
```
representing the inequality $a_1 x + a_2 y + a_3 z \leq b$.

The intersection of all half-space constraints forms the polyhedron.
