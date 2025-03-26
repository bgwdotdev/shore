{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              gleam
              erlang_27
              zig
            ];
            ERL_LIB = "${pkgs.erlang_27}/lib/erlang/lib";
            LD_LIBRARY_PATH = "${pkgs.erlang_27}/lib/erlang/usr/include";
          };
        }
      );
}
