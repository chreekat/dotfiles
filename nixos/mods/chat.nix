{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Electron can't infer a keyring from XDG_CURRENT_DESKTOP=none+notion.
    # Wrapped rather than overridden: overriding commandLineArgs forces a
    # local rebuild of Element.
    (pkgs.symlinkJoin {
      name = "element-desktop";
      paths = [ pkgs.element-desktop ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/element-desktop --add-flags --password-store=gnome-libsecret
      '';
    })
    signal-desktop
    slack
    # not cached?!?
    # telegram-desktop
  ];
}
