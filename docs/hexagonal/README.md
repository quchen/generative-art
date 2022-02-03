# Hexagonal coordinates

The best resource on the web to basic hexagonal coordinate systems is
[Red Blob Games][hex-article] interactive (!) writeup on the topic.

We use cubical coordinates here. This is what the hexagonal plane looks like,
with the coordinates `r` `s` and `t` filled in. Note that `r+s+t=0`, always.

Hexagons allow an interesting discrete view on things, so in the following
picture, we have a blue line and a circle of radius 2 around (-1, 1, 0) in
yellow.

![](1_line_and_circle.svg)

[hex-article]: https://www.redblobgames.com/grids/hexagons/

One reason hexagons are really cool is because it almost doesn’t matter what
you’re doing with them, the result looks pretty interesting – the following is
just a 2D histogram of Gaussian values on a hexagonal coordinate system:

![](gaussian_hexagons.svg)
