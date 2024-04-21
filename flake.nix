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
        rm *.aux *.pdf *.log *.synctex.gz *.glo *.glsdefs *.ist *.syg
      '';
    in pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
        texliveMinimal
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
