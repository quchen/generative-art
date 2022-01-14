# Poisson Disc Sampling

Do create a uniform-looking coverage of the plane, just using random points does
not always yield the best results: Although the points are technically uniformly
distributed, they don't _look_ that uniform.

Poisson disc sampling yields much better-looking results compared to random
points:

![](poisson-disc.svg)

Poisson discs are random points that are no closer to each other than a certain
distance `r`. Effectively, they correspond to little discs around each point
that set a minimum distance. The parameter `k` is used to tweak accuracy vs.
performance: Choosing a large `k` leads to a more uniform-looking and
tighter-packed distribution, but leads to a lot more samples being tried and
rejected. In our modified version of the algorithm, a good choice is `k = 4`.

The algorithm works as follows:

* We keep a list of active samples (that still can have additional neighbouring
  points), and a grid of all samples we already found. The grid spacing is
  chosen so that per grid cell there cannot be more than one sample per grid
  cell.

* We start with taking a random point as initial sample.

* The next sample added by choosing an active sample at random, and generating
  `k` points evenly spaced around the sample at distance `r`. Using the grid, we
  look for the first point that does not have any neighbouring samples in
  distance smaller than `r`. If we find one, then we add this point to our
  active samples. If none of the generated points match the criteria, then we
  assume we can't find any more neighbouring points, and we retire the sample
  from the active samples.

    * The original algorithm uses random points at a distance between `r` and
      `2*r`. This leads to a lot more rejection (typically, `k = 30` is chosen
      for a good coverage), and therefore much worse performance.

* This is repeated until there are no more active samples.
