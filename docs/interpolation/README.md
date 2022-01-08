# Interpolation

## Making rough things smoooth

Interpolation can be used for giving crude shapes a smooth look. Here is an
example in which only the red dots are given, and the algorithm smoothly joins
them with Bezier curves. The control points are shown in yellow/blue.

![](1_bezier_open.svg)

Or here is a version of Picasso’s squirrel approximated with a handful of
points.

![](2_picasso_squirrel.svg)


## Output compression

SVG files can become quite big. One way to compress them while maintaining the
same visual fidelity is approximating the contained curves with simpler ones.

The original curve in blue has very high resolution, and below we simplify the
data with gradually increasing coarseness. When we drop points that are
basically all on a line (depending on a parameter) and then re-join the points
smoothly, we can save quite a number of points without impacting visual fidelity
at all.

![](3_simplify_path.svg)

Another trick is to subdivide the source curve, forgetting newly created unnecessary points again, and interpolating fresh Bezier curves through the remainder. This can achieve a number of effects beyond smoothing or saving data, such as a more hand-sketched appearance, depending on the parameters.

![](4_bezier_subdivide.svg)
