# Plan

Point-in-time plan. Each milestone ends in something demoable. Facts marked
(verified) were read from primary sources; (secondary) come from forum and
blog summaries and must be re-checked against the DCS install before being
relied on.

## Fixed inputs

- Thinkpad: kuusi, panel `eDP` 1920x1080 at 60 Hz, 309x173 mm (verified,
  `xrandr`).
- Windows: `smilga`, Windows 11, NVIDIA GPU (so Sunshine's encoder is
  `nvenc`), one monitor, 3440x1440, primary, at the origin (verified,
  `[System.Windows.Forms.Screen]::AllScreens`). The DCS window is therefore
  5360x1440 and the virtual monitor's columns start at `x = 3440`.
- DCS: the Steam build at
  `C:\Program Files (x86)\Steam\steamapps\common\DCSWorld`, 2.9.30 MT;
  user files in `C:\Users\b\Saved Games\DCS` (verified, `dcs.log`).
- 2D only. No VR.
- Network: both machines on Tailscale; `tailscale status` on smilga shows
  kuusi as `direct` over the LAN (verified). Moonlight connects to `smilga`
  by its Tailscale name. A `relay` entry would mean the latency budget is
  blown and the LAN route needs fixing first.
- Multiplayer on integrity-checked servers is the end goal. Intermediate
  steps may break the integrity check; the finished setup must not.
  Milestones 1-3 are integrity-clean by construction: the MPD viewports are
  hooked by the unmodified module, and nothing is installed outside Saved
  Games.
- Viewport hooks in the module, verified by grepping the install for
  `try_find_assigned_viewport`: `LEFT_MFCD` and `RIGHT_MFCD` in
  `Mods\aircraft\AH-64D\Cockpit\Scripts\Displays\MFD\indicator\LCD\MFD_LCD.lua`,
  and `TEDAC` (falling back to `CENTER_MFCD`) in
  `...\Displays\TEDAC\TEDAC_viewport_cfg.lua`. Nothing else: the EUFD and KU
  have no hook, so exporting their pixels means editing module Lua, which
  fails the integrity check.
- Pilot seat only. CPG is a later project; when it comes, the TEDAC exports
  integrity-clean via the hook above.
- The panel is not a touchscreen: Innolux N140HCG-GQ2, a matte non-touch
  part (verified from the eDP EDID; no touch controller in ACPI or on USB).
  Bezel-button input would need hardware and is out of scope.

## Design

Chosen: virtual monitor on Windows, captured and streamed as a whole.

- Virtual Display Driver (VirtualDrivers/Virtual-Display-Driver, MIT).
  Installed via `winget install --id=VirtualDrivers.Virtual-Display-Driver -e`;
  resolutions live in `C:\VirtualDisplayDriver\vdd_settings.xml` (verified
  from its README). One mode: 1920x1080 at 60 Hz. The driver is a plain
  IddCx monitor, so DCS sees it at launch like any other display and no
  launch ordering is needed.
- Sunshine on Windows. `output_name` set to the virtual display's
  `device_id` from `%ProgramFiles%\Sunshine\tools\dxgi-info.exe`;
  `dd_configuration_option = ensure_active` so the stream re-enables the
  display if Windows dropped it; `dd_resolution_option = auto` so the display
  is switched to whatever Moonlight asks for; `encoder = nvenc` (all
  verified from Sunshine's configuration.md). Audio stays on the host
  (`--audio-on-host` on the client).
- moonlight-qt on NixOS (in nixpkgs, built with VA-API; verified). CLI
  verified from its source:
  `moonlight stream smilga Desktop --display-mode fullscreen
  --resolution 1920x1080 --fps 60 --bitrate N --audio-on-host --quit-after`.
  kuusi has an AMD GPU, so decode is VA-API.
- DCS: borderless window at the combined resolution, a monitor-setup Lua in
  `Saved Games\DCS\Config\MonitorSetup\` with `Viewports.Center` covering the
  main monitor and `LEFT_MFCD`/`RIGHT_MFCD` inside the virtual monitor's
  rectangle; `UIMainView` and `GU_MAIN_VIEWPORT` pinned to Center so menus
  stay on the main screen. Fullscreen cannot exceed the primary display, so
  borderless is mandatory (secondary). The window's origin is the primary
  display's origin, so the virtual monitor is arranged in Windows directly
  to the right of the main one, top-aligned, and its viewports get
  `x = main_width` (secondary, ~80% confident; Milestone 2 step 1 confirms).
- Panel layout: `windows/MonitorSetup/ThinkpadMPD.lua`. The two MPDs fill
  the top 960 rows of the panel, leaving a 1920x120 strip at the bottom for
  Milestone 4's EUFD; the window's bottom 360 rows under the panel fall on
  no monitor at all, which is harmless.

Alternatives, and why not first:

- Apollo (Sunshine fork with a built-in virtual display, SudoVDA). Removes the
  separate driver install, but creates the display when the stream starts and
  removes it when the stream ends, so DCS must be launched after Moonlight
  connects every time. Also unclear (~60%) whether it extends rather than
  replaces the desktop. Fallback if the Virtual Display Driver misbehaves.
- Render the MPDs inside the main monitor and stream a cropped region
  (OBS + NDI). No driver, but costs main-screen area, and NDI on NixOS is
  awkward. Fallback if no virtual-display route works.
- Export MPD *data* instead of pixels (Export.lua / DCS-BIOS) and render it
  ourselves. Not possible for MPDs: pages, TADS video and the map are rendered
  in-engine and `list_indication` exposes only text indications. Viable for
  the EUFD and KU, which are text displays -- see Milestone 4.

## Milestone 1: the Thinkpad shows a Windows virtual monitor

Tracer bullet. No DCS involved. Demo: a stopwatch window dragged onto the
virtual monitor appears on the Thinkpad; a photo of both screens gives the
glass-to-glass latency.

1. Windows: `windows/install.ps1`, rerun after each step it hands back.
   It installs the Virtual Display Driver package, copies
   `windows/vdd_settings.xml` (one 1920x1080@60 mode) and creates the
   `Root\MttVDD` device with the package's own devcon; only the monitor's
   placement right of the main one is by hand, and the script checks it.
   Check: a window dragged off the right edge disappears.
2. Windows: the same script installs Sunshine, merges `windows/sunshine.conf`
   into the live config, takes the virtual display's `device_id` from
   Sunshine's startup log for `output_name`, and copies the DCS preset into
   Saved Games. Pairing is by hand: `moonlight pair smilga` on kuusi, the
   PIN into Sunshine's web UI.
3. NixOS: `nixos/moonlight.nix`, imported by kuusi in
   `nixos/systems/flake.nix`. Adds `pkgs.moonlight-qt` and a `dcs-mpd` script
   wrapping the `moonlight stream` invocation with the host, resolution,
   fps and bitrate baked in. Check: the demo above, plus the latency
   number recorded here.

## Milestone 2: Apache MPDs on the Thinkpad

Demo: in the AH-64D in a free-flight mission, both MPDs appear on the
Thinkpad, the main view is unchanged, menus stay on the main screen, and the
fps hit is measured.

1. Select `ThinkpadMPD` in DCS options, with the resolution and window
   mode its header names. Check: the viewports land where expected; fix `x`
   if the window origin assumption was wrong.
2. Tune and measure: DCS fps with and without the export, Sunshine fps and
   bitrate (MPD content is mostly static, so expect low bitrate to suffice),
   H.264 vs HEVC decode on kuusi, and the Moonlight overlay's latency figure.
   Numbers go in README.md.

## Milestone 3: one action per side

Demo: on the Thinkpad, one keybinding (notion) brings up the MPDs fullscreen
and quitting the stream returns to the desktop; on Windows, nothing beyond
launching DCS as usual, because the virtual monitor is always present.

1. notion binding and desktop entry for `dcs-mpd`; Moonlight's own
   quit shortcut documented in README.md.
2. README.md "Flying" section: the full ritual, both sides, plus what to do
   when the stream drops mid-flight.

## Milestone 4 (stretch): EUFD in the free strip

The EUFD is a text display, so the integrity-safe route is to render it on
kuusi from exported data rather than export its pixels: Export.lua sends
`list_indication` text for the EUFD over UDP, and a small program on kuusi
renders it in the 120 px strip with Moonlight running
borderless rather than fullscreen. Speculative until someone checks what
`list_indication` returns for the EUFD device. The pixel route
(`AH64_PLT_EUFD` via module Lua edits) is a single-player stepping stone
only.
