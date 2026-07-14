{ pkgs, ...}:
let
  # Handy tool for tracking works in progress
  bugs =
    let
      losh-t = pkgs.python3Packages.buildPythonApplication {
        pname = "losh-t";
        format = "setuptools";
        version = "1.2.0";
        src = fetchGit {
          url = "https://github.com/sjl/t";
        };
      };
    in
    pkgs.writeScriptBin "b" ''
      set -Eeou pipefail
      # Prefer a committed top-level 'bugs' file; otherwise fall back to the
      # git dir (so repos can keep the list either in-tree or hidden).
      top=$(git rev-parse --show-toplevel)
      if [ -f "$top/bugs" ]; then
          taskDir=$top
      else
          taskDir=$(git rev-parse --git-common-dir)
      fi
      run() { ${losh-t}/bin/t --task-dir "$taskDir" --list bugs "$@"; }
      if [ "$#" -gt 0 ] && [ "$1" = "-i" ]; then
          # inspect: show the single bug (open or done) matching an id prefix.
          # losh-t has no show-one command, and bare `b <id>` would ADD a task,
          # so intercept -i here and grep the verbose listing by id prefix.
          shift
          id="$1"
          run -v | grep -i "^$id" \
              || { echo "b: no bug matching id: $id" >&2; exit 1; }
      else
          run "$@"
      fi
    '';
in {
  environment.systemPackages = [ bugs ];
}
