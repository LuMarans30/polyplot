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
representing the inequality a₁x + a₂y + a₃z ≤ b.

The intersection of all half-space constraints forms the polyhedron.