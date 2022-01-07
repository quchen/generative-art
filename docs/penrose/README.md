# Penrose tiling

[Penrose tiling](https://en.wikipedia.org/wiki/Penrose_tiling) is an aperiodic
tiling: It covers the plane without gaps or overlap, but the resulting grid does
not repeat itself.

There are three different types of Penrose tilings:

* Pentagonal tiling (P1) with four shapes: Pentagons, stars, boats, and
  diamonds.
* Kites and darts (P2) with two shapes, kites and darts.
* Rhombic tiling (P3) with two shapes, a thin and a thick rhombus.

Our implementation is based on the P3 tiling.

## Tiling rules

There are a few rules to follow in order to reach an aperiodic tiling. These
rules are encoded on the tiles with circles: When tiling the plane, the circles
must align:

![](1_base_configurations.svg)

## Subdivision

While a penrose tiling can be constructed by starting with a tile and then
adding more and more tiles, this is rather difficult. The much easier option is
to follow a set of subdivision rules which split a large tile into smaller
tiles.

![](2_subdivision.svg)

Note that the smaller tiles don't exactly fit into the larger tile, but some of
them are cut in half. They will however perfectly align with the other half from
the neighbouring tile.

## Other tilings

The different types of Penrose tilings can be converted between each other. Our
implementation is based on P3, but P1 can be retrieved by just adding a few
lines on the tiles:

![](3_inscribed_pentagons.svg)

Of course, they follow the same subdivision rules:

![](4_subdivision_with_pentagons.svg)
