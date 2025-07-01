{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/default";
    crane.url = "github:ipetkov/crane";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    {
      self,
      nixpkgs,
      systems,
      crane,
      rust-overlay,
    }:
    let
      eachSystem =
        f:
        nixpkgs.lib.genAttrs (import systems) (
          system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ (import rust-overlay) ];
            };

            craneLib = (crane.mkLib pkgs).overrideToolchain (pkgs: pkgs.rust-bin.stable."1.88.0".default);

            self' = self // {
              packages = self.packages.${system} or { };
              devShells = self.devShells.${system} or { };
              checks = self.checks.${system} or { };
              apps = self.apps.${system} or { };
              formatter = self.formatter.${system}; 
            };
          in
          f {
            inherit
              system
              pkgs
              craneLib
              self'
              ;
          }
        );

      workspace.root = ./.;
      workspace.name = "swls";
      workspace.version = self.sourceInfo.shortRev or self.sourceInfo.lastModifiedDate;
    in
    {
      packages = eachSystem (
        {
          craneLib,
          self',
          ...
        }:
        let
          inherit (self'.packages) swls;

          cargoArtifacts = craneLib.callPackage ./nix-support/cargo-artifacts.nix { inherit workspace; };
        in
        {
          default = swls;

          swls = craneLib.callPackage ./nix-support/workspace-member-package.nix {
            inherit workspace cargoArtifacts;
            member = ./swls;
          };

          lsp-web = craneLib.callPackage ./nix-support/workspace-member-package.nix {
            inherit workspace cargoArtifacts;
            member = ./lsp-web;
          };
        }
      );

      formatter = eachSystem ({ pkgs, ... }: pkgs.nixfmt-rfc-style);

      devShells = eachSystem (
        {
          craneLib,
          self',
          pkgs,
          ...
        }:
        {

          default = craneLib.devShell {
            pname = "${workspace.name}-dev";
            inputsFrom = pkgs.lib.unique (builtins.attrValues self'.packages);
            packages = [
              pkgs.rust-analyzer
              pkgs.taplo
              pkgs.nixd
              self'.formatter
            ];
          };
        }
      );

    };

}
