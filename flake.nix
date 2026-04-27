{
  description = "gamelauncher";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs: let
    overlay = final: prev: {
      haskell =
        prev.haskell
        // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev
            // {
              gamelauncher = hfinal.callCabal2nix "gamelauncher" ./. {};
            };
        };
      gamelauncher = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.gamelauncher;
    };
    perSystem = system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [overlay];
      };
      hspkgs = pkgs.haskellPackages;
    in {
      devShell = hspkgs.shellFor {
        withHoogle = true;
        packages = p: [p.gamelauncher];
        buildInputs = [
          hspkgs.cabal-install
          hspkgs.haskell-language-server
          hspkgs.hlint
          hspkgs.ormolu
          pkgs.bashInteractive
        ];
      };
      defaultPackage = pkgs.gamelauncher;
      packages.default = pkgs.gamelauncher;
      overlays.default = final: prev: {
        inherit (pkgs) gamelauncher;
      };
    };
  in
    {inherit overlay;} // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
