{pkgs, ...}:
let
  emacsNotes = pkgs.emacs.pkgs.withPackages (epkgs: (with epkgs.melpaStablePackages; [
    use-package
    god-mode
  ]));
in {
  environment.systemPackages = [
    emacsNotes
  ];
}
