{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # need to match Stackage LTS version from stack.yaml resolver
        hPkgs = pkgs.haskell.packages."ghc8107";

        devtools = with hPkgs; [
          ghc
          ghcid
          # ormolu # <- seems broken
          hlint
          hoogle
          haskell-language-server
          retrie
        ];

        pkgconfigDeps = with pkgs; [
          cairo.dev
          gtk2.dev
          gtkd
          pkgconfig
          zlib
          zlib.dev
        ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = pkgconfigDeps ++ devtools ++ [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            ln -s $out/lib/pkgconfig/zlib.pc $out/lib/pkgconfig/z.pc
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
                --extra-lib-dirs=$out/lib \
                --extra-include-dirs=$out/include \
              " \
              --set PKG_CONFIG_PATH $out/lib/pkgconfig \
              --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath pkgconfigDeps}
          '';
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = pkgconfigDeps ++ devtools ++ [ stack-wrapped ];

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath pkgconfigDeps;
        };
        packages = {
          default = stack-wrapped;
        };
      });
}
