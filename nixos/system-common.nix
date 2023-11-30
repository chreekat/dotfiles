{ lib, pkgs, ... }:

let
  patchedExtracturl = pkgs.extract_url.overrideAttrs (old: {
    patches = (old.patches or []) ++ [ ./extracturl.patch ];
  });
  rekey =
    # Actual keyboard config
    #
    # Add some Nordic characters to an otherwise US-Dvorak layout.
    #
    # Run at the beginning of an X Session (see below). Bundled as a script so I
    # can rerun it when I plug in a keyboard.
    #
    # Fuck me, right?
    # TODO: Box drawing chars
    pkgs.writeShellScriptBin "rekey"
      ''
        <<EOF cat | ${pkgs.xorg.xkbcomp}/bin/xkbcomp - $DISPLAY &>/dev/null
        xkb_keymap {
                xkb_keycodes  { include "evdev+aliases(qwerty)"	};
                xkb_types     { include "complete"	};
                xkb_compat    { include "complete"	};
                partial xkb_symbols   {
                    include "pc+us(dvorak)+inet(evdev)"
                    include "ctrl(nocaps)+compose(lctrl)+level3(ralt_switch)"

                    key <AE04> { [ NoSymbol, NoSymbol, EuroSign, sterling ] };
                    key <AD01> { [ NoSymbol, NoSymbol, aring, Aring ] };
                    key <AD11> { [ NoSymbol, NoSymbol, dead_acute ] };
                    key <AC01> { [ NoSymbol, NoSymbol, adiaeresis, Adiaeresis ] };
                    key <AC02> { [ NoSymbol, NoSymbol, odiaeresis, Odiaeresis ] };
                    key <AC03> { [ NoSymbol, NoSymbol, eacute, Eacute ] };
                    key <AB01> { [ NoSymbol, NoSymbol, Greek_lambda, NoSymbol ] };
                };
                xkb_geometry  { include "pc(pc104)"	};
        };
        EOF
      '';

  # Handy tool for tracking works in progress
  bugs =
    let
      losh-t = pkgs.python3Packages.buildPythonApplication {
        pname = "losh-t";
        version = "1.2.0";
        src = fetchGit {
          url = "https://github.com/sjl/t";
        };
      };

    in
    pkgs.writeScriptBin "b" ''
      set -Eeou pipefail
      topLevel=$(git rev-parse --git-common-dir)
      ${losh-t}/bin/t --task-dir $topLevel --list bugs $@
    '';
in
{
  imports = [
    ./nix-direnv.nix
    ./mods/tailscale.nix
    ./mods/backlight.nix
    ./mods/haskell-platform-lite.nix
    ./mods/web-dev.nix
    ./mods/language-server.nix
    ./mods/dev.nix
    ./mods/chat.nix
    ./mods/nitrokey.nix
  ];
  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    # Don't use tmpOnTmpfs, because I actually use all that ram when compiling
    # Haskell
    tmp.useTmpfs = false;
    tmp.cleanOnBoot = true;
  };

  # 20.03
  console.useXkbConfig = true;
  # 19.09
  # i18n.consoleUseXkbConfig = true;

  # Include man section 3. >:(
  documentation.dev.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # categories suck
      anki
      chrysalis
      aspell
    # 3d-printing
      cura
    # personal admin tools
      bup
      keepassxc
      pass
      hledger
      hledger-ui
    # development
      bench
      bugs
      cachix
      difftastic
      dhall
      direnv
      entr
      ghcid
      git
      git-crypt
      niv
      nix-diff
      nix-prefetch
      nix-prefetch-docker
      nix-prefetch-github
      nix-prefetch-scripts
      ripgrep
      tmux
      universal-ctags
      vim_configurable
      vscodium
    # databases
      beekeeper-studio # db tool
      postgresql
    # media
      aegisub
      audacity
      # beets - broken dep python3.7-soco in 20.03
      #digikam
      blueberry # bluetooth tray and gui
      bluetuith # bluetooth tui
      ghostscript
      gimp
      gitAndTools.git-annex
      graphviz
      gv
      handbrake # Rips DVD to video files
      imagemagick
      inkscape
      pinta # Simple image editor
      spotify
      transmission
      vlc
      (callPackage ./terminal-image-viewer {})
      zathura # pdf viewer
    # linux
      (sox.override { enableLame = true; })
      bc # More like dc amirite
      eplot # Fast command line plotter
      fd
      file
      fzf
      gdb
      gnumake
      htop
      jq
      jre
      lshw
      man-pages # OBVIOUSLY
      mosh
      ncdu
      nix-bash-completions
      pandoc
      par
      pciutils # lspci
      python3
      qdirstat
      sqlite-interactive
      sshuttle
      tree
      unzip
      usbutils
      yq
    # Xorg (in concert with enabling xmonad)
      albert # Launcher, bound to ScrollLock
      arandr
      flameshot
      keynav
      notify-osd
      pavucontrol
      rekey
      xfce.thunar # File browser
      xcape
      xclip
      xorg.xev
      xorg.xmessage
    # Gifcasts (FIXME: make vokoscreen make gifs by default)
      screenkey # show keys in gif casts
      slop # Used by screenkey to select a region
      vokoscreen # gif casts
    # Xorg tray
      cbatticon
      networkmanagerapplet
    # Web
      chromium
      firefox
      newsboat
      w3m
      wget
      youtube-dl
    # Email
      patchedExtracturl
      lmdb # Header cache for neomutt
      neomutt
      notmuch
      offlineimap
      thunderbird
      vcal
    # networking
      bind
      nethogs
      nmap
      # Broken on 20.09 with "urwid-2.1.1 not supported for interpreter
      # python2.7 :(
      # speedometer
      tcpdump
    # devops
      awscli
      dive
      kubectl
      kubectx
      minikube
      metal-cli
      freerdp
      sops
      # nixops --> libvirt insecure
    ];

  # Set up the default environment
  environment.variables = {
    EDITOR = "vim";
  };

  fonts = {
    fonts = [
      pkgs.FSD-Emoji-font
      pkgs.pragmataPro-font
      pkgs.fira-mono
      pkgs.noto-fonts-emoji
      pkgs.noto-fonts
      pkgs.siji
      pkgs.comic-neue
    ];
    fontconfig.defaultFonts = {
      monospace = [ "PragmataPro Mono" ];
      emoji = [
        "FSD Emoji"
      ];
    };
  };

  hardware = {
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
  };

  i18n.defaultLocale = "en_GB.UTF-8";

  location = {
    provider = "geoclue2";
    #latitude = 60.2443;
    #longitude = 24.8800;
  };

  networking.networkmanager = {
    enable = true;
  };

  nix = {
    gc = {
      automatic = true;
      dates = "monthly";
      # Bumped from 2w to 4w on the 50th of March 2020 because channels were
      # moving slowly.
      options = "--delete-older-than 30d";
    };
    settings = {
      # Needed for various good things
      trusted-users = ["b"];
      experimental-features = [ "nix-command" "flakes" ];
    };
  };

  # Sorry, RMS
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ./nonfree-fonts)
  ];

  ## Configure programs.
  programs = {
    atop = {
      enable = true;
      netatop.enable = true;
      atopgpu.enable = true;
    };
    bash.enableCompletion = true;
    gnupg.agent = {
      enable = true;
    };
    ssh.startAgent = true;
    xss-lock = {
      enable = true;
      extraOptions = [
        "--notifier=${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
        "--transfer-sleep-lock"
      ];
      lockerCommand =
        lib.concatStringsSep " " [
          "env XSECURELOCK_PASSWORD_PROMPT=disco"
          "    XSECURELOCK_BLANK_TIMEOUT=10"
          "    XSECURELOCK_BLANK_DPMS_STATE=off"
          # Need to escape the % because they get interpreted by systemd.
          "    XSECURELOCK_DATETIME_FORMAT='%%a %%d %%b %%Y, %%R %%Z, W%%V'"
          "    XSECURELOCK_SHOW_DATETIME=1"
          "${pkgs.xsecurelock}/bin/xsecurelock"
        ];
    };
  };

  security.sudo.wheelNeedsPassword = false;

  services = {
    autorandr.enable = true;
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/" ];
    };
    dictd = {
      enable = true;
      DBs = ((d: [
        d.wiktionary
        d.wordnet
      ]) pkgs.dictdDBs);
    };
    fprintd.enable = true;
    fwupd.enable = true;
    keybase.enable = true;
    kbfs.enable = true;

    # /run/user/1000 limit
    logind.extraConfig = "RuntimeDirectorySize=50%";

    openssh.enable = true;

    printing.enable = true;

    # Redshift + Geoclue
    redshift = {
        enable = true;
        brightness.night = "0.97";
        temperature = {
          day = 6500;
          night = 4000;
        };
    };

    syncthing = {
      enable = true;
      user = "b";
      group = "b";
      dataDir = "/home/b/Syncthing";
      configDir = "/home/b/Syncthing/.config/syncthing";
    };

    tzupdate.enable = true;


    # Packages that include udev rules
    udev.packages = [ pkgs.chrysalis ];

    unclutter-xfixes.enable = true;

    urxvtd.enable = true;

    # Enable and configure the X11 windowing system.
    xserver = {
      enable = true;
      autoRepeatDelay = 300;
      autoRepeatInterval = 10;
      libinput.enable = true;
      windowManager.notion = {
        enable = true;
      };
      desktopManager.xfce = {
        enable = true;
      };

      ## X KEYBOARD MAP

      # Basic keyboard setup that gets reused by the console via
      # i18n.consoleUseXkbConfig.
      layout = "dvorak";
      xkbOptions = "ctrl:nocaps";

      # NixOS' support for xkeyboard-config has a high impedance mismatch.
      # See the definition for rekey above.
      displayManager.sessionCommands = "rekey";
    };
  };


  system = {
    autoUpgrade = {
      enable = true;
      dates = "12:30";
    };
  };

  # Don't need to wait for wired connection, yo
  systemd.network.wait-online.anyInterface = true;


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups.b = {
    gid = 1000;
  };
  users.users.b = {
    isNormalUser = true;
    uid = 1000;
    group = "b";
    extraGroups = ["i2c" "users" "wheel" "video" "systemd-journal" "docker" "transmission"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzNy6bOnkcu54nPeN523uvfRq3WbGCgEbTQifWLF+D0 b@kuusi"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5wfyVQYW4sBkZ7a9dNlrGorUXQc1BvHmhxBmmrrwAKs/kkvaS1jgraDvsjo+xo4skiI9HwVXP7OMhjjdLH6MMk3X0Sewv+V8V6xDmpr6NYZKgmFx9b0tPp4IBRsuZu5vbNDyIQo/DbNFzCKT2+ax5DKy/GBahH1WMq4Ks+N01sBrXlOl40fnihdcYTpEZ5jJYheAjrgKpcmoAV364C4lNFfUGxkYVM0DAs39yIahDWQ7bReuzaJRhz+CpfAfNLTpIU1vnWP0yv4ZSRM47eQ59dYVieLqZFxroJ1xXfkdSR10AjCdcmOZWa9Bx9qBF5I5kCdPbW559D6QaROsy4EUzqGOuY8ynALm0zJDXrEj5rTxV8vJfHjQ+m8+oXUdRxdJtbrf0al8mytD80hl50mdzlhomWFiEJO9ny2N4AZiJJ0xbIhAFvHAjaTwTa+j6uF0OFIobcqrtaGjjBYXG8FPdJCNByt7N7g8UOTggfzQoxf3g3dBx79gfBbuKFEohTI7XvgEAvFFVrW3NHn7eYN4QKfqO51ns6atmJDIo0Phpjnz0670I+Od9WETa/IJcNjJkcA22612hhRtNK5NZcgKTRijNStegFAtY7ZApB2nMj5lQcRA3Dps6lF1PpICIj5IBF9R4n5AeR405l9QG113iqvAIezHgYHSltOgHBmGOMw== b@fuzzbomb"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICXYHLQLP5E9vLzXPaht5rM6T5V7VtFyb48Ep2VCI2Nn bryan@omena"
    ];
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  xdg.mime.defaultApplications = {
    "application/pdf" = "zathura.desktop";
  };

}
