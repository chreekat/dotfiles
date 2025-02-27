{pkgs, ...}:
let
  emacsNotes = pkgs.emacs.pkgs.withPackages (epkgs: (with epkgs.melpaStablePackages; [
    use-package
  ]));
in {
  environment.systemPackages = [
    emacsNotes
  ];
}
