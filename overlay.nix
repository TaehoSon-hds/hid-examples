final: prev: with final; {

  myHaskellPackages = prev.haskellPackages.override (old: {
    overrides = lib.composeManyExtensions [
      (old.overrides or (_: _: {}))
      (hfinal: hprev: with haskell.lib; {
        hid-examples = hfinal.callCabal2nix "hid-examples" ./. {};
      })
    ];
  });

  hid-examples = haskell.lib.justStaticExecutables myHaskellPackages.hid-examples;

}
