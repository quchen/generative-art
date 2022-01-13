# Voronoi diagrams

A [Voronoi diagram](https://en.wikipedia.org/wiki/Voronoi_diagram) is generated
by distributing a set of points (seeds) on the plane, and then coloring the
region (cell) around each seed that is closer to this seed than to any other
seeds.

Here's a simple example of a Voronoi diagram with five seeds:

![](3_full_voronoi.svg)

## Construction

In our algorithm, a Voronoi diagram is constructed iteratively: Given a set of
seeds and cells, we iteratively add another seed, create a new cell, and update
the other cells.

Starting with two initial seeds and cells, the two are merged in the following
way:

![](1_cut_polygon.svg)

Adding a third seed works by creating an initial cell around the new seed,
limiting that cell to the other seeds, and updating the existing cells with the
new seed.

![](2_add_polygon.svg)
