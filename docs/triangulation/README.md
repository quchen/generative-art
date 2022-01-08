# Triangulation

Triangulation divides a polygon into a number of triangles.

![](1_square.svg)
![](2_regular_polygon.svg)

Triangulations often make things easier to handle. For example, you may not know
the formula to calculate the area of a polygon (involving
[determinants of adjacent point pairs!][poly-area]). But if you know the area of
a triangle, then you can calculate the area by summing up the area of the
triangulated pieces.

![](3_haskell_logo.svg)
![](4_spiral.svg)

[poly-area]: http://mathworld.wolfram.com/PolygonArea.html
