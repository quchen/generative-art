Zeit: 1.5h

Einleitung (20min, realistisch 30min)
=====================================



Übersicht geben
---------------



### Cooler Showcase: Voronoi3D

Haskell-Logo wird zerschnitten.



### Zu erklärende Konzepte:

    * Rechner ist Werkzeug, der exakt und viel machen kann. Leider ist er völlig unkreativ.
    * Komplexes Bild = Vervielfachung einfacher Schritte
    * Zufall



### Toolstack vorstellen

An sich sprachagnostisch, aber wir verwenden hier:

    * Haskell
    * Cairo (Vektorgrafik)
    * Randomness: MWC-Random, Noise (Perlin)
    * Own Library (Geometry, etc.)
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



### Paar mehr Showcases zeigen

Intention: »Schönes Bild« -> Breakdown -> »Das könnte ich ja selber machen!«



Mitcoden
--------



Ein Showcase zum Mitcoden (ca. 1h, 3x 20min)
* Block 1:
    * Vorgegeben: Main, die weißen Canvas rendert
    * Zeigen: Shatter-Prozess, Polygone zeichnen mit Cairo
    * Aufgabe:
        * Das selbst machen
        * Alle Polygone ein bisschen kleiner machen
* Block 2: Randomness dazubringen
    * Zeigen: RNG, alle Scherben zufällig drehen
    * Aufgabe: Das machen
    * Optional: Perlin-Noise für mehr Kohärenz, zusätlich zu drehen auch zufällig verschieben
* Block 3: Farbe, Variation Liniienbreite, etc.
    * Zeigen: Farbschemata, Alpha, Blending, …
    * Aufgabe: Zufällig einfärben/Farben zufällig ändern, etc.
* Am Ende von jedem Block: Show&Tell, Bilder irgendwo einsenden und zeigen
* Letzte 5min, falls Zeit: Mögliche weitere Ideen, wie man das Beispiel noch fortsetzen kann
    * Effekte örtlich variieren, zB Zerfall von L->R



Preparation
--------------------------------------------------------------------------------

Prerequisites: You should have beginner knowledge of Haskell, being somewhat fluent with simple types, pattern matching, function application and composition, basic `IO` and `do`-notation. ["Learn you a haskell"](http://learnyouahaskell.com/)-level should be sufficient.

To prepare for the workshop, you should have a working `stack` setup. To install `stack`, follow the instructions on https://docs.haskellstack.org/en/stable/README/. To make sure everything is working, clone [github.com/quchen/generative-art](https://github.com/quchen/generative-art), and run `stack build`. This will take some time the first time you run it for compiling all the dependencies, so we recommend doing it once before the workshop!
