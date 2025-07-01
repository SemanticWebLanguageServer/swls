{
  lib,
  workspace,
  craneLib,
  pkg-config,
  openssl,
  llvmPackages,
}@inputs:
craneLib.buildDepsOnly (
  (import ./common-arguments.nix inputs)
  // {
    pname = "${workspace.name}-workspace";
    version = workspace.version;
  }
)
