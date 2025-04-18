{
  description = "Flake to build + develop moreComplexHeatmap R package";

  nixConfig = {
    bash-prompt = "\[moreComplexHeatmap$(__git_ps1 \" (%s)\")\]$ ";
  };

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

	dirfns = {
	  url = "github:kewiechecki/dirfns?ref=master";
	  flake = true;
      inputs.nixpkgs.follows = "nixpkgs";
	};
  };

  outputs = { self, nixpkgs, flake-utils, dirfns }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs  = import nixpkgs { inherit system; config.allowUnfree = true; };
        rpkgs = pkgs.rPackages;
        io = dirfns.defaultPackage.${system};

        myPkg = rpkgs.buildRPackage rec {
          name    = "moreComplexHeatmap";
          version = "0.0.4";
          src     = ./.;

          # R‐side dependencies:
          propagatedBuildInputs = [
              rpkgs.circlize
              rpkgs.ComplexHeatmap
          ];

          nativeBuildInputs = [
            pkgs.R
            pkgs.pkg-config
          ];

          # C‑library dependencies
          buildInputs = [
          ];

		  preBuild = ''
		  make build
		  mv build $out
		  '';

          # re‑enable Nix’s R-wrapper so it injects R_LD_LIBRARY_PATH
          dontUseSetLibPath = false;

          meta = with pkgs.lib; {
            description = "…";
            license     = licenses.mit;
            maintainers = [ maintainers.kewiechecki ];
          };
        };
      in rec {
        # 1) allow `nix build` with no extra attr:
        defaultPackage = myPkg;

        # 2) drop you into a shell for interactive R work:
        devShells = {
          default = pkgs.mkShell {
            name = "moreComplexHeatmap-shell";
            buildInputs = [
              pkgs.git
              pkgs.R
			  io
              rpkgs.circlize
              rpkgs.ComplexHeatmap
            ];
            shellHook = ''
source ${pkgs.git}/share/bash-completion/completions/git-prompt.sh
            '';
          };
        };
      });
}

