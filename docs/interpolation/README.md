# Interpolation

## Making rough things smoooth

Interpolation can be used for giving crude shapes a smooth look. Here is an
example in which only the red dots are given, and the algorithm smoothly joins
them with Bezier curves. The control points are shown in yellow/blue.

![](../haddock/Geometry/Bezier/bezierSmoothen.svg)

Or here is a version of Picasso’s squirrel approximated with a handful of
points.

![](2_picasso_squirrel.svg)

We can also do this for closed trajectories with a trick: each Bezier curve is
only influenced by its left and right neighbouring point. We can take the three
first points of the loop and duplicate it at its end, and we’ll get a smooth
interpolation with some elements overlapping, because we took part of the curve
twice. We can then remove the bezier curves that correspond to these overlaps
and get a perfectly smooth result. Can you see where the loop’s start is in this
picture? I can’t.

![](bezier_loop_interpolation.svg)

## Output compression

SVG files can become quite big. One way to compress them while maintaining the
same visual fidelity is approximating the contained curves with simpler ones.

The original curve in blue has very high resolution, and below we simplify the
data with gradually increasing coarseness. When we drop points that are
basically all on a line (depending on a parameter) and then re-join the points
smoothly, we can save quite a number of points without impacting visual fidelity
at all.

We provide two path simplifiers:

  - [Ramer-Douglas-Peucker][rdp] eliminates points that are close to the
    connecting line between two other points

  - [Visvalingam–Whyatt][vw] eliminates points that span a triangle with very
    small area with its neighbours

Deciding which one is better is best done by experiment.

[vw]: https://en.wikipedia.org/wiki/Visvalingam%E2%80%93Whyatt_algorithm
[rdp]: https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm

### Ramer-Douglas-Peucker

![](3_simplify_path_rdp.svg)

### Visvalingam–Whyatt

![](3_simplify_path_vw.svg)

### Radial

![](3_simplify_path_radial.svg)

## Other applications

Another trick is to subdivide the source curve, forgetting newly created unnecessary points again, and interpolating fresh Bezier curves through the remainder. This can achieve a number of effects beyond smoothing or saving data, such as a more hand-sketched appearance, depending on the parameters.

![](4_bezier_subdivide.svg)
