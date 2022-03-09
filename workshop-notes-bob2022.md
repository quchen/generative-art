Zeit: 1.5h

Introduction (20…30 min)
================================================================================

Before we start
--------------------------------------------------------------------------------

* Agenda: Ca. 30min intro+showcases, ca. 1h code-along
* Beore we start, did everyone compile the repo? If not, now is your chance to
  run `stack build --haddock`!

What is generative art? (David)
--------------------------------------------------------------------------------

* Creating art with computer support
* Giving _some_ control to the computer, but keeping in control of the
    overall design
* As opposed to:
    * Computer art -> full control
    * Things like action painting -> full randomness
* Computers are great tools, but completely uncreative
* Main ingredients:
    * 2D graphics lib (cairo)
    * Randomness
    * Splitting the image into small parts, and repeat them to form a complex
      image (iteration, recursion)
    * Human creativity :-)

First showcase: 3D Voronoi (Franz)
--------------------------------------------------------------------------------

* Start with random points
* Voronoi tesselation around those points
* Pure randomness does not look that good ⇒ Poisson-Disc distribution
    instead
* Add colors
* Add random heights, and isometric perspective

Second showcase: Flow fields (David)
--------------------------------------------------------------------------------

TODO

Overview over toolstack (David)
--------------------------------------------------------------------------------

* Haskell
    * You could use any language, we're using Haskell b/c we like it :-)
* Cairo (2D vector graphics)
* Randomness: MWC-Random, Noise (Perlin)
* Own Library: Walk through most important modules
    * Open via `stack haddock --open`
    * Geometry.Core:
        * 2D Vectors
        * Lines
        * Polygons
        * Angles
        * Bounding Boxes
        * Transformations
    * Draw:
        * Drawing Presets
        * Colors
        * SVG and PNG file handling
    * Geometry.Algorithms.Cut


Workshop example (Franz)
================================================================================

Our example: Shattering a square
Mode:
    * Show concept (5min)
    * code alone or in small groups (10min)
    * Show & Tell
        * Attach your pictures to github.com/quchen/generative-art/issues/4
        * Or, if you're not no Github: Host it somewhere, and send a link to
          quchen via gather chat

TODO:
* Prepare example picture, attach to issue

Block 1: Cutting a square (Franz)
--------------------------------------------------------------------------------

* Prerequisite: `main` rendering a white canvas
* Show + code along:
    * Set up IDE + stack
    * Draw a square
    * Cut the square into two parts
    * Make a random cut
* Task:
    * Make several cuts
    * There are different possibilities to orchestrate the cuts – use your
      fantasy:
        * Fixed number of cuts, select a random shard
        * Fixed number of cuts, select the largest shard
        * Cut as long as all parts are below a maximum size
        * Optionally add acceptance criteria, like "shard sizes should only
          differ by <= 20%"
* (Show & tell)

Block 2: Geometric transformations (Franz)
--------------------------------------------------------------------------------

* Show + code along:
    * Random number generator that rotates all shards by a random amount
* Task:
    * Play around with transformations (rotate, translate, resize, …)
    * You can use Perlin noise for more coherence
    * Or make the transformation dependent on coordinates, i.e. left-to-right,
      center-to-border
* (Show & tell)

Block 3: Color (Franz)
--------------------------------------------------------------------------------

* Show + code along:
    * Color schemes, alpha, blending
* Task:
    * Use randomness to colorize the shards
    * Be creative!
* Show & Tell

Preparation
================================================================================

Prerequisites: You should have beginner knowledge of Haskell, being somewhat fluent with simple types, pattern matching, function application and composition, basic `IO` and `do`-notation. ["Learn you a haskell"](http://learnyouahaskell.com/)-level should be sufficient.

To prepare for the workshop, you should have a working `stack` setup. To install `stack`, follow the instructions on https://docs.haskellstack.org/en/stable/README/. To make sure everything is working, clone [github.com/quchen/generative-art](https://github.com/quchen/generative-art), and run `stack build --haddock` for building all dependencies and generating the documentation. This will take some time the first time you run it, so we recommend doing it once before the workshop! The [`cairo`](https://hackage.haskell.org/package/cairo) Haskell library depends on the `cairo` or `gtk` system package, so you might need to install either (see [github.com/gtk2hs/gtk2hs](https://github.com/gtk2hs/gtk2hs)).

If you're looking for an IDE to write Haskell code: We can recommend VisualStudio Code with the [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) plugin
