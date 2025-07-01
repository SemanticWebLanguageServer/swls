{
  lib,
  workspace,
  craneLib,
  pkg-config,
  openssl,
  llvmPackages,
  ...
}:
{
  src = lib.fileset.toSource {
    inherit (workspace) root;

    fileset = lib.fileset.unions [
      (craneLib.fileset.commonCargoSources workspace.root)
      (lib.fileset.fileFilter (file: file.hasExt "ttl") workspace.root)
      (lib.fileset.fileFilter (file: file.hasExt "json") workspace.root)
    ];
  };

  strictDeps = true;

  nativeBuildInputs = [
    pkg-config
    llvmPackages.clang
    llvmPackages.bintools
  ];

  buildInputs = [ openssl ];
}
