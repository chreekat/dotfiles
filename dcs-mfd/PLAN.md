# Plan

Point-in-time plan. Each milestone ends in something demoable. Facts marked
(verified) were read from primary sources; (secondary) come from forum and
blog summaries and must be re-checked against the DCS install before being
relied on.

## Fixed inputs

- Thinkpad: kuusi, panel `eDP` 1920x1080 at 60 Hz, 309x173 mm (verified,
  `xrandr`).
- Windows: NVIDIA GPU, so Sunshine's encoder is `nvenc`.
- 2D only. No VR.
- Network: both machines on Tailscale, which takes the direct LAN path when
  one exists. `tailscale status` on kuusi should show the Windows peer as
  `direct`; a `relay` entry means the latency budget is blown and the LAN
  route needs fixing first.
- Multiplayer on integrity-checked servers is the end goal. Intermediate
  steps may break the integrity check; the finished setup must not.
  Milestones 1-3 are integrity-clean by construction: `LEFT_MFCD` and
  `RIGHT_MFCD` need no module edits, and nothing is installed outside Saved
  Games.
- Pilot seat only. CPG (and the TEDAC) is a later project.
- Bezel-button input is a planned milestone, not a stretch goal.
- The panel is NOT a touchscreen: it is an Innolux N140HCG-GQ2, a matte
  non-touch part (verified: panel model read from the eDP EDID; no touch
  controller exists in any ACPI table or on USB). Pressing bezel buttons
  therefore needs added hardware -- see "Bezel input" below.

## Inputs still needed

1. Main Windows monitor resolution. It sets the combined DCS resolution and
   the `x` offset of the MFD viewports.
2. The authoritative viewport names, from the install itself:

       findstr /s /n try_find_assigned_viewport "<DCS>\Mods\aircraft\AH-64D\Cockpit\Scripts\*.lua"

   Secondary sources agree that `LEFT_MFCD` and `RIGHT_MFCD` work for the
   AH-64D unmodified and follow whichever seat is occupied. Names reported for
   the other displays (`AH64_PLT_EUFD`, `AH64_CPG_EUFD`, `AH64_PLT_KU`,
   `AH64_CPG_KU`, `AH64_TEDAC`) only exist after editing module Lua, which
   fails the integrity check.

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
  `moonlight stream <tailscale-name> Desktop --display-mode fullscreen
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
- Panel layout. Two MPD screens side by side, each a square, with a black
  ring around each where the physical bezel's buttons sit. A 70 px ring gives
  820x820 screens: 70 + 820 + 70 = 960 per MFD across, and 960 of the 1080
  rows, leaving a 120 px strip for Milestone 5's EUFD. The ring width is a
  placeholder until the bezel hardware is chosen (its cutout dictates the
  square); the Lua and the input map share these numbers, so they live in
  one place once Milestone 4 starts.
- Bezel input. The panel is not touch, so bezel buttons need hardware in
  front of the screen. Candidates, decided at Milestone 4:
  - Thrustmaster MFD Cougar bezels: two USB frames of 20 buttons each, made
    for exactly this. Plugged into the Windows machine they bind in DCS's
    own controls and need no code at all; plugged into kuusi they need an
    evdev-to-DCS-BIOS forwarder (Haskell: read the frame's evdev node, map
    button to DCS-BIOS command, send to its UDP command port over
    Tailscale). The screen squares must match the frame's cutout -- measure
    before committing to a layout. DCS-BIOS lives in Saved Games
    `Export.lua`, which the integrity check does not cover (secondary,
    ~85%); its AH-64D support includes the MPD buttons (secondary, ~85%).
  - Swap in the touch variant of this panel (the T14s Gen 1 shipped with an
    on-cell touch FHD option). A hardware project: new panel and likely a
    different LCD cable; only then does a touchscreen plan (exclusive
    `EVIOCGRAB` so Moonlight never sees the touches, touch-to-button by
    rectangle, DCS-BIOS over UDP) apply.
  Moonlight-forwarded clicks are no route in either case: exported viewports
  are not clickable in DCS (secondary).

Alternatives, and why not first:

- Apollo (Sunshine fork with a built-in virtual display, SudoVDA). Removes the
  separate driver install, but creates the display when the stream starts and
  removes it when the stream ends, so DCS must be launched after Moonlight
  connects every time. Also unclear (~60%) whether it extends rather than
  replaces the desktop. Fallback if the Virtual Display Driver misbehaves.
- Render the MFDs inside the main monitor and stream a cropped region
  (OBS + NDI). No driver, but costs main-screen area, and NDI on NixOS is
  awkward. Fallback if no virtual-display route works.
- Export MFD *data* instead of pixels (Export.lua / DCS-BIOS) and render it
  ourselves. Not possible for MFDs: pages, TADS video and the map are rendered
  in-engine and `list_indication` exposes only text indications. Viable for
  the EUFD and KU, which are text displays -- see Milestone 5.
- Drawing bezel buttons on kuusi as an overlay window. Unnecessary given the
  on-screen labels, and an X11 overlay under notion on top of a fullscreen
  SDL window is its own project.

## Milestone 1: the Thinkpad shows a Windows virtual monitor

Tracer bullet. No DCS involved. Demo: a stopwatch window dragged onto the
virtual monitor appears on the Thinkpad; a photo of both screens gives the
glass-to-glass latency.

1. Windows: install the Virtual Display Driver with the one 1920x1080@60
   mode; arrange it right of the main monitor. Check: a window dragged off
   the right edge disappears into it. Commit `windows/vdd_settings.xml`.
2. Windows: install Sunshine; set `output_name`, `dd_configuration_option`,
   `dd_resolution_option`, `encoder`; pair the Thinkpad over its Tailscale
   address. Commit the settings as `windows/sunshine.conf` (only the keys we
   set, never the state file with pairing secrets).
3. NixOS: `nixos/moonlight.nix`, imported by kuusi in
   `nixos/systems/flake.nix`. Adds `pkgs.moonlight-qt` and a `dcs-mfd` script
   wrapping the `moonlight stream` invocation with the host, resolution,
   fps and bitrate baked in. Check: the demo above, plus the latency number
   and the `tailscale status` path recorded here.

## Milestone 2: Apache MFDs on the Thinkpad

Demo: in the AH-64D in a free-flight mission, both MFDs appear on the
Thinkpad, the main view is unchanged, menus stay on the main screen, and the
fps hit is measured.

1. `windows/MonitorSetup/ThinkpadMFD.lua` with Center, LEFT_MFCD, RIGHT_MFCD,
   UIMainView, GU_MAIN_VIEWPORT, using the panel layout above, with a copy
   step into Saved Games (by hand, or `windows/install.ps1` once there are
   three files to copy). DCS options: resolution = main + 1920 wide,
   borderless, monitors = this preset. Check: the viewports land where
   expected; fix `x` if the window origin assumption was wrong.
2. Tune and measure: DCS fps with and without the export, Sunshine fps and
   bitrate (MFD content is mostly static, so expect low bitrate to suffice),
   H.264 vs HEVC decode on kuusi, and the Moonlight overlay's latency figure.
   Numbers go in README.md.

## Milestone 3: one action per side

Demo: on the Thinkpad, one keybinding (notion) brings up the MFDs fullscreen
and quitting the stream returns to the desktop; on Windows, nothing beyond
launching DCS as usual, because the virtual monitor is always present.

1. notion binding and desktop entry for `dcs-mfd`; Moonlight's own
   quit shortcut documented in README.md.
2. `windows/install.ps1`: copies the monitor-setup Lua and the Sunshine
   settings into place, idempotently.
3. README.md "Flying" section: the full ritual, both sides, plus what to do
   when the stream drops mid-flight.

## Milestone 4: press the MFD bezels

Demo: pressing a bezel button beside an on-screen label presses that button
in the cockpit, in multiplayer, with Moonlight still fullscreen.

1. Choose the hardware (see "Bezel input" above) and resize the Milestone 2
   layout to its cutout.
2. Simplest wiring first: bezels on the Windows machine, bound in DCS's own
   controls. If that holds, this milestone is hardware plus a layout tweak
   and steps 3-5 vanish.
3. Otherwise (bezels on kuusi, or a touch panel): spike, no code committed.
   Install DCS-BIOS on Windows, confirm from its AH-64D control reference
   that the MPD bezel buttons are exposed, and send one press by hand with
   `nc -u` from kuusi. This settles the two secondary facts above before
   anything is built on them.
4. `bezel/` Haskell project: read the device's evdev node, print events.
   Pure core: `eventToButton :: Layout -> InputEvent -> Maybe Button` with
   the layout rectangles shared with the Lua (generate the Lua from the
   Haskell layout, or the other way round; pick when here).
5. Send `Button` presses as DCS-BIOS commands over UDP; a NixOS user service
   or a wrapper so `dcs-mfd` starts and stops it with the stream.
6. Rocker and knob controls on the MPD (brightness, video, the page rockers)
   as a second pass if the button mapping holds up.

## Milestone 5 (stretch): EUFD in the free strip

The EUFD is a text display, so the integrity-safe route is to render it on
kuusi from exported data rather than export its pixels: Export.lua sends
`list_indication` text for the EUFD over UDP, and a small program on kuusi
renders it in the 120 px strip with Moonlight running
borderless rather than fullscreen. Speculative until someone checks what
`list_indication` returns for the EUFD device. The pixel route
(`AH64_PLT_EUFD` via module Lua edits) is a single-player stepping stone
only.
