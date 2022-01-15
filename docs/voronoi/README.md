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

## Delaunay Triangulation

The geometric dual to Voronoi diagrams is Delaunay triangulation: The Voronoi
seeds are the vertices of a Delaunay triangulation, and each Voronoi edge
bisects a Delaunay edge in a right angle.

![](delaunay_random.svg)

This means that every Delaunay triangulation can be converted to a Voronoi
tesselation:

![](delaunay_voronoi.svg)

## Centroidal Voronoi diagrams / Lloyd Relaxation

A centroidal Voronoi diagram is a Voronoi diagram where all seeds are in the
centroid (or center of mass) of their corresponding region. Centroidal Voronoi
tesselations are quite common in nature, like the Giant's Causeway, or the cells
in the Cornea.

One way to approximate a centroidal Voronoi diagram is Lloyd relaxation or
Lloyd's algorithm. It works by simply moving each seed into the centroid of its
region. Here's four iterations of Lloyd relaxation; the red arrows show the
distance between the seed and the centroid.

![](lloyd_relaxation.svg)
