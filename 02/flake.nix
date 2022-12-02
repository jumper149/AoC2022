{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }: {

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      pkgs.mkShell {
        buildInputs = [
          pkgs.chez
        ];
        shellHook = ''
          export PATH=$PATH:~/.pack/bin/
        '';
      };

  };
}
