{pkgs, ...}:
let
  emacsNotes = pkgs.emacs.pkgs.withPackages (epkgs:
    let p = epkgs.melpaStablePackages;
    in [
      #p.ws-butler # "Server does not allow request for unadvertised object d3927f6131f215e9cd3e1f747be5a91e5be8ca9a" 2025-09-15
    ]);
in {
  environment.systemPackages = [
    emacsNotes
  ];
}
