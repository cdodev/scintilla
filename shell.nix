{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;


  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  hsPkgs = haskellPackages.override { overrides = self: super: rec {
    opaleye   = self.callPackage ./opaleye {};
    tisch     = self.callPackage ./tisch { inherit opaleye; };
    scintilla = self.callPackage ./. { inherit tisch opaleye; };
    };
  };
  drv = hsPkgs.scintilla;


in

  if pkgs.lib.inNixShell then drv.env else drv
