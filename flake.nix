{
  description = "OCaml development environment";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:

    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages = {
        # nix run .#dev 
        dev = pkgs.writeShellScriptBin "dev" ''
          cd "$(git rev-parse --show-toplevel)"
          dune exec ocaml_c_compiler
        '';
      };

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [ ocaml ocamlformat opam ] ++
          (with pkgs.ocamlPackages; [ dune_3 odoc findlib core base fmt ppx_inline_test ppx_expect ppx_sexp_conv ppx_deriving sexplib sexplib0 ]);

        shellHook = ''
          ${pkgs.ocaml}/bin/ocaml --version
        '';
      };
    });
}
