{ config, lib, pkgs, ... }:

let
  rekey =
    # Actual keyboard config
    #
    # Add some Nordic characters to an otherwise US-Dvorak layout.
    #
    # Run at the beginning of an X Session (see below). Bundled as a script so I
    # can rerun it when I plug in a keyboard.
    #
    # Fuck me, right?
    pkgs.writeShellScriptBin "rekey"
      ''
        <<EOF cat | ${pkgs.xorg.xkbcomp}/bin/xkbcomp - $DISPLAY &>/dev/null
        xkb_keymap {
                xkb_keycodes  { include "evdev+aliases(qwerty)"	};
                xkb_types     { include "complete"	};
                xkb_compat    { include "complete"	};
                xkb_symbols   {
                    include "pc+us(dvorak)+inet(evdev)"
                    include "ctrl(nocaps)+compose(lctrl)+level3(ralt_switch)"
                    include "eurosign(4)"

                    key <AD01> { [ NoSymbol, NoSymbol, aring, Aring ] };
                    key <AD11> { [ NoSymbol, NoSymbol, dead_acute ] };
                    key <AC01> { [ NoSymbol, NoSymbol, adiaeresis, Adiaeresis ] };
                    key <AC02> { [ NoSymbol, NoSymbol, odiaeresis, Odiaeresis ] };
                    key <AB01> { [ NoSymbol, NoSymbol, Greek_lambda, NoSymbol ] };
                };
                xkb_geometry  { include "pc(pc104)"	};
        };
        EOF
      '';

  # Handy tool for tracking works in progress
  bugs =
    let
      losh-t = pkgs.pythonPackages.buildPythonApplication {
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
  imports = [ ./lorri/direnv/nixos.nix ];
  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    # Don't use tmpOnTmpfs, because I actually use all that ram when compiling
    # Haskell
    tmpOnTmpfs = false;
    cleanTmpDir = true;
  };

  # Include man section 3. >:(
  documentation.dev.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    (with pkgs.haskellPackages; [
      cabal-install
      cabal2nix
      fast-tags
      # Haskell Platform-lite
      (ghcWithPackages (p: with p; [
        QuickCheck
        containers
        nonempty-containers
        pretty-simple
        text
        megaparsec
        template-haskell
        servant
        scalpel-core
        ]))
      hasktags
      hlint
      hledger
      hledger-ui
      hpack
      stack
      stylish-haskell
      (callPackage /home/b/Projects/usort/package.nix {})
    ]) ++ (with pkgs; [
      # categories suck
        anki
        drive
      # personal admin tools
        bup
        keepassxc
        pass
      # development
        bugs
        cachix
        entr
        universal-ctags
        ghcid
        git
        (import ./lorri {})
        nix-prefetch-scripts
        nix-prefetch
        nix-prefetch-github
        ripgrep
        tmux
        vim_configurable
      # media
        audacity
        beets
        digikam
        ghostscript
        gimp
        gitAndTools.git-annex
        graphviz
        gv
        handbrake # Rips DVD to video files
        imagemagick
        inkscape
        # tor-browser-bundle-bin
        # ^ error: cannot download tor-browser-linux64-9.0.2_en-US.tar.xz from any mirror
        transmission
        vlc
      # linux
        (sox.override { enableLame = true; })
        atop
        bc
        fd
        file
        fzf
        gdb
        gnumake
        htop
        jq
        jre
        lshw
        manpages # OBVIOUSLY
        ncdu
        nix-bash-completions
        pandoc
        par
        python
        qdirstat
        sqlite
        sshuttle
        tree
        unzip
        yq
      # Xorg (in concert with enabling xmonad)
        arandr
        dmenu
        flameshot
        networkmanagerapplet
        notify-osd
        pavucontrol
        rekey
        trayer
        xcape
        xclip
        xorg.xev
        xorg.xmessage
      # Web
        chromium
        firefox
        w3m
        wget
        youtube-dl
      # social
        gnupg1compat
        keybase-gui
        mumble
        signal-desktop
        thunderbird
        weechat
        wire-desktop
      # networking
        bind
        nethogs
        nmap
        speedometer
        tcpdump
      # devops
        awscli
        dive
        kubectl
        kubectx
        minikube
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

  fonts.fonts = [ pkgs.fira-mono pkgs.noto-fonts-emoji pkgs.noto-fonts ];
  fonts.fontconfig.defaultFonts.monospace = [ "Fira Mono" ];

  hardware = {
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
  };

  i18n.consoleUseXkbConfig = true;
  location.provider = "geoclue2";

  networking.networkmanager = {
    enable = true;
    packages = [
      pkgs.openconnect_pa
    ];
  };

  nix = {
    gc = {
      automatic = true;
      dates = "11:30";
      options = "--delete-older-than 2w";
    };
    # Needed for various good things
    trustedUsers = ["b"];
    # Let commands use these caches if they want.
    trustedBinaryCaches = [
      "http://devdatabrary2.home.nyu.edu:5000/"
    ];
    binaryCachePublicKeys = [
      "devdatabrary2.home.nyu.edu-1:xpI1XOvf7czNv0+0/1ajpgotpOnUMTUBBF9v97D5/yk="
      "databrary.cachix.org-1:jOz34d80mzekR2pjkK9JCczPi2TKeifQ/OHYcg8I6tg="
    ];
  };

  ## Configure programs.
  programs = {
    bash.enableCompletion = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
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
          "${pkgs.xsecurelock}/bin/xsecurelock"
        ];
    };
  };

  services = {
    autorandr.enable = true;
    fwupd.enable = true;
    keybase.enable = true;
    kbfs.enable = true;
    localtime.enable = true;

    printing.enable = true;

    # Redshift + Geoclue
    redshift = {
        enable = true;
        temperature = {
          day = 6500;
          night = 6000;
        };
    };

    # Allow the video group to change backlight brightness
    udev.extraRules = ''
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';

    unclutter-xfixes.enable = true;

    urxvtd.enable = true;

    # Enable and configure the X11 windowing system.
    xserver = {
      enable = true;
      autoRepeatDelay = 300;
      autoRepeatInterval = 10;
      libinput.enable = true;
      multitouch = {
        enable = true;
        ignorePalm = true;
      };
      wacom.enable = true;
      windowManager.notion = {
        enable = true;
      };
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = p : [ p.lens ];
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


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraGroups.b = {
    gid = 1000;
  };
  users.users.b = {
    isNormalUser = true;
    uid = 1000;
    group = "b";
    extraGroups = ["users" "wheel" "video" "systemd-journal"];
  };
}
