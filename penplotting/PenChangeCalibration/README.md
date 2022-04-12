Nonius
======

Recalibrating the plotter precisely after a pen change is not trivial. This
little calibration picture can help to pin-point the offset required for a new
pen to 0.1mm accuracy.

The workflow is as follows:

1. Paint the test picture somewhere outside of the canvas. Use a different
   coordinate system so you don’t mess up your initial calibration (e.g. using
   G54.1).
2. Revert to standard coordinates (G54)
3. Paint your picture
5. Switch pen
6. Recalibrate Z
7. Draw nonius in G54.1 coordinates again
7. Take note of the XY offset
8. Change G54 coordinates to compensate the offset
9. Draw with second color
