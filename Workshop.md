



Zeit: 1,5h


Einleitung (20min, realistisch 30min):
* Ein kleiner Showcase, Appetizer
* Ggf. etwas mit Parametern herumspielen
* Breakdown in Primitive
* Dabei die Konzepte erklären
    * Einen Teil des Schaffensprozesses an den Rechner abgeben
    * Bild aus kleinen Einheiten oder Algorithmen zusammensetzen
    * Randomness
* Toolstack
    * Haskell
    * Cairo (Vektorgrafik)
    * Randomness: MWC-Random, Noise (Perlin)
    * Own Library (Geometry, etc.)
* In principle language–agnostic, could be Python as well

* A few more showcases
    * Intention: »What a nice picture« -> breakdown -> »I could do that myself«

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








TODOs:
* Ein Showcase für die Intro
* 2–3 Showcases aufbereiten
* (/) Haupt-Showcase finden: Polygone zerschneiden
    * TODO: Was zerschneiden wir?



Potentielle große Themen:
* Vektorfelder, …
    * (-) man muss Konzepte erklären
* Voronoi
    * (+) Einfach erzeugt (API), recht wandelbar
    * (-) Konzept schwerer erklärbar
* Linien schneiden, shattering
    * (+) Einfach erzeugt (API), recht wandelbar
* Tiling (Penrose/Truchet)
    * (-) etwas langweilig
* Mondrian
