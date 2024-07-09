{
  description = "How To Prove It Notes Shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.x86_64-linux.default = let
      cleanup = pkgs.writeShellScriptBin "cleanup" ''
        rm *.aux *.pdf *.log *.synctex.gz *.glo *.glsdefs *.ist *.syg *.acn *.acr *.alg *.glg *.fls *.fdb_latexmk *.gls *.slg *.syi
      '';
    in pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
        (texliveSmall.withPackages (ps: with ps; [ latexmk hypdoc gensymb glossaries glossaries-extra ]))
        texlab
        zathura
        tectonic
      ];
      buildInputs = with pkgs; [
        cleanup
      ];
    shellHook = ''
      export HELIX_RUNTIME="$PWD/runtime"
    '';
    };
  };
}
