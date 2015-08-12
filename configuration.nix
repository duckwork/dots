# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
imports =
  [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

# Use the GRUB 2 boot loader.
boot.loader.grub.enable = true;
boot.loader.grub.version = 2;
# Define on which hard drive you want to install Grub.
boot.loader.grub.device = "/dev/sda";

networking.hostName = "nixos"; # Define your hostname.
networking.hostId = "12d6588b";
# networking.wireless.enable = true;  # Enables wireless.
  # ALSO: run `wpa_passphrase ESSID PSK > /etc/wpa_supplicant.conf`

# Select internationalisation properties.
i18n = {
  consoleFont = "Lat2-Terminus16";
  consoleKeyMap = "us";
  defaultLocale = "en_US.UTF-8";
};

programs = {
  bash.enableCompletion = true;
};

environment = {
  systemPackages = with pkgs; [
    # web
    wget
    curl
    surfraw
    chromium
    uzbl
    gnash
    irssi
    # X utilities
    xclip
    xfontsel
    compton
    termite
    dzen2
    # image stuff
    gimp
    feh
    imagemagick
    sxiv
    mpv
    # utilities
    vimHugeX
    # neovim
    silver-searcher
    tmux
    ranger
    plan9port
    # dropbox dropbox-cli
    # programming & development
    bashCompletion
    gitAndTools.gitFull
    ctags
    gnumake
    gcc
    python
    ruby
    rubygems
    ] ++ (with haskellPackages; [
    basePrelude
    ghc
    # Development
    ghcMod
    hasktags
    codex
    hscope
    pointfree
    pointful
    hoogle
    stylishHaskell
    hlint
    cabalInstall
    # other stuff
    alex
    haddock
    async
    dmenu
    yeganesh      # small wrapper for dmenu
    xmobar        # statusbar for xmonad
    xmonadExtras
    pandoc
    pandocCiteproc
    pandocTypes
    regexPosix
    regexApplicative
    regexPcre
  ]);

  shellAliases = {
  ll = "ls -l";
  la = "ls -A";
  vinix = "sudoedit /etc/nixos/configuration.nix && sudo nixos-rebuild switch";
  renix = "sudo nixos-rebuild switch";
  shutdown = "systemctl poweroff";
  };

  variables = {
    SUDO_EDITOR = "${pkgs.vim}/bin/vim";
    EDITOR = "${pkgs.vim}/bin/vim";
    VISUAL = "${pkgs.vim}/bin/vim";
    BROWSER = "${pkgs.uzbl}/bin/uzbl-browser";
  };
};

nixpkgs.config = {
  allowUnfree = true;

  # firefox = {
  #   enableAdobeFlash = true;
  # };

  chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };

  vim = {
    python = true;
    lua = true;
    ruby = true;
    ftNixSupport = true;
  };
};

fonts = {
  enableFontDir = true;
  enableGhostscriptFonts = true;
  fonts = with pkgs; [
    corefonts
    inconsolata
    ubuntu_font_family
    comic-neue
    eb-garamond
    fira
    libertine
    opensans-ttf
    proggyfonts
    terminus_font
    ttf_bitstream_vera
    vistafonts
  ];
};

# Enable automatic nixos pkg garbage collection
nix.gc.automatic = true;
nix.gc.dates = "03:15";

# List services that you want to enable:
services = {
  openssh.enable = true;
  printing.enable = true;   # CUPS
  locate = {
    enable = true;
    period = "30 19 * * *"; # run updatedb at 10:30 PM daily
    output = "/var/cache/locatedb";
    localuser = "root";
  };
  xserver = {
    enable = true;
    autorun = true;
    layout = "us";
    synaptics.enable = true;
    desktopManager = {
      xterm.enable = true;
      default = "none";
    };
    displayManager = {
      slim.enable = true;
      slim.defaultUser = "case";
      slim.autoLogin = true;
      sessionCommands = ''
        ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
        ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ~/img/wallpaper.jpg &
        ${pkgs.compton}/bin/compton &
      '';
    };
    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
      default = "xmonad";
    };
  };
  # acpid = {
  #   enable = true;
  #   lidEventCommands = "";
  #   powerEventCommands = "";
  # };
  # bitlbee.enable = true;
};

users.extraUsers.case =
{ isNormalUser = true;
  createHome = true;
  home = "/home/case";
  extraGroups = [ "wheel" ];
  useDefaultShell = true;
};

time.timeZone = "America/Phoenix";

}
