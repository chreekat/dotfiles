{pkgs, ...}:
let
  emacsNotes = pkgs.emacs.pkgs.withPackages (epkgs: (with epkgs.melpaStablePackages; [
    use-package
    ws-butler
  ]));
in {
  environment.systemPackages = [
    emacsNotes
  ];
}
