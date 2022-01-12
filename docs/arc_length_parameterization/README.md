# Arc length parameterization

The canonical representation of Bezier curves is not parameterized with curve
length, but by an arbitrary parameter. Evenly spacing points along a Bezier
curve with respect to curve length requires a surprising amount of calculation.

In the picture, the same Bezier curve has been added dots to. The upper one has
them evenly spaced by line length, while the lower one shows the naive result
when evenly varying the standard Bezier parameter.

![](1_single_curve.svg)
