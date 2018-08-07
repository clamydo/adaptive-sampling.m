# adaptive-sampling.m
A adaptive 2D subdivision algorithm to approximate the zero-isoline of a
function.

# Algorithm
The algorithm subdivides a cell into four sub-cells if the sign at the four grid
points changes. If the sampled function is continuous this implies the zero
transition lies within this cell. Starting from an intitial regular rectengular
grid the algorithm automatically refines the grid as needed to a given depth.

# Usage
For an usage example see the provided `example.nb` Mathematica notebook.

# License
The license of this code is the GPLv3. If you should publish work using
this code you must give credit.
