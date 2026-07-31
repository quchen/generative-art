# Generative Art

## Layout

### `src/`: The core library

Library with geometry definitions and algorithms to create artworks.

### `showcases/` and `penplotting/`: The artworks

Each artwork is a separate stack project that depends on the library.

To build an artwork, run the corresponding stack commands in the artwork's directory.

There are two ways an artwork can depend on the library:

1. Via relative path: `../..` in `stack.yaml`. This is for artworks under active development, since the library can be adapted/extended during development of the artwork.
2. Via commit hash in `stack.yaml`. This is for archived artworks, so that the artwork can still be built with the library version it was created with, even if the library changed since then.

## Tooling

This is a `stack` project. It uses `hpack` to automatically generate *.cabal files from package.yaml files.

Do **NOT** try to run `cabal` commands, always use `stack`.
Do **NOT** edit any *.cabal files. Edit the package.yaml instead.

Relevant commands:
* Building the library: `stack build` in the main directory
* Building an artwork: `stack build` in the artwork's directory
* Running the test suite: `stack test :testsuite` in the main directory
