{ config, lib, pkgs, ... }:

{
  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  networking.networkmanager.enable = true;

  fonts.fonts = [ pkgs.fira-mono pkgs.fira-code pkgs.open-dyslexic ];
  fonts.fontconfig.defaultFonts.monospace = [ "Fira Mono" ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    (with pkgs.haskellPackages; [
      cabal-install
      cabal2nix
      # codex ## broken in 18.03
      fast-tags
      ghc
      hasktags
      hledger
      hledger-ui
      hpack
      # hpack-convert ## BUSTED, lol?
      pandoc
      stack
      # stack2nix ## BUSTED lol
      threadscope
    ]) ++ (with pkgs; [
      # categories suck
        anki
        bup
        freecad
        nethack
        shutter
        # steam ## Issues with libvulkan
      # development
        bats
        bfg-repo-cleaner
        bench
        universal-ctags
        git
        ripgrep
        tmux
        vim_configurable
        xcape
        virtualbox
        vagrant
      # media
        beets
        digikam
        ghostscript
        graphviz
        imagemagick
        gimp
        inkscape
        ktorrent
        mendeley
        vlc
        handbrake # Rips DVD to video files
        audacity
        gitAndTools.git-annex

      # linux
        (sox.override { enableLame = true; })
        bc
        bind
        binutils
        enscript
        fd
        file
        fzf
        gdb
        gnumake
        gparted
        gv
        html-tidy
        htop
        jq
        jre
        lshw
        moreutils # vidir and other goodies
        mpw
        ncdu
        nix-bash-completions
        nmap
        nmap
        openssl
        pandoc
        par
        python
        qdirstat
        sqlite
        sshuttle
        tree
        unzip
        xclip
        xorg.xev
        # yq # Missing from 17.09
      # Web
        chromium
        firefox
        w3m
        wget
        youtube-dl
      # social
        gnupg1compat
        mumble
        thunderbird
        weechat
      # devops
        awscli
        nixops
      # .deb is missing as of 2018-09-27 (for 17.09)
        # tor-browser-bundle-bin
        # skypeforlinux # .deb is missing as of 2018-09-27
        # signal-desktop # Missing from 17.09
    ]);

  # Set up the default environment
  environment.variables = {
    EDITOR = "vim";
    PARINIT = "rTbgqR B=.,?_A_a Q=_s>|";
  };

  ## Configure programs.
  programs.bash.enableCompletion = true;

  # Enable gpg-agent on login
  programs.ssh.startAgent = true;

  # Redshift + Geoclue
  services.geoclue2.enable = true;
  services.redshift = {
    enable = true;
    provider = "geoclue2";
    #latitude = "40.67";
    #longitude = "-73.98";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraGroups.b = {
    gid = 1000;
  };
  users.extraUsers.b = {
    isNormalUser = true;
    uid = 1000;
    group = "b";
    extraGroups = ["users" "wheel" "vboxusers" "docker"];
  };

  # Automatic updates.
  system.autoUpgrade.enable = true;
}
