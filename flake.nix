{
  description = "tasty-tutorial";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              tastyTutorial = hfinal.callCabal2nix "tasty-tutorial" ./. { };
            };
        };
        tastyTutorial = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.tastyTutorial;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; config = { allowUnfree = true; };};
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.tastyTutorial ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              pkgs.bashInteractive
              (pkgs.vscode-with-extensions.override {
                vscode = pkgs.vscodium;
                vscodeExtensions = with pkgs.vscode-extensions; [
                  dracula-theme.theme-dracula
                  haskell.haskell
                  jnoortheen.nix-ide
                  justusadam.language-haskell
                  mkhl.direnv
                ];
                }
              )
            ];
          };
          defaultPackage = pkgs.tastyTutorial;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}