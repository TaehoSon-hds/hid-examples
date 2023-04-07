{ pkgs }: with pkgs; let

in haskellPackages.shellFor {
  packages = p: [ hid-examples ];
  buildInputs =
    (with haskellPackages;
    [ haskell-language-server

    ]) ++
    [ ];
}
