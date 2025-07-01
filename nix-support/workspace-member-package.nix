{
  lib,
  member,
  workspace,
  craneLib,
  pkg-config,
  openssl,
  llvmPackages,
  cargoArtifacts,
}@inputs:
let
  inherit (craneLib.crateNameFromCargoToml { cargoToml = "${member}/Cargo.toml"; }) pname version;
in
craneLib.buildPackage (
  (import ./common-arguments.nix inputs)
  // {
    inherit pname version;

    cargoExtraArgs = "-p ${pname}";

    inherit cargoArtifacts;
  }
)
