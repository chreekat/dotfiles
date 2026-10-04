# Plan

Point-in-time plan. Each milestone ends in something demoable. Facts marked
(verified) were read from primary sources; (secondary) come from forum and
blog summaries and must be re-checked against the DCS install before being
relied on.

## Inputs still needed

Answers change the numbers in Milestones 1 and 2, not their shape.

1. Thinkpad panel: `xrandr` output on kuusi (resolution, refresh rate), and
   whether kuusi is in fact the Thinkpad in question.
2. Windows machine: GPU vendor (picks the Sunshine encoder: `nvenc`,
   `amdvce`, `quicksync`), main monitor resolution, Windows 10 or 11.
3. 2D or VR? Everything below assumes 2D. Exported viewports under VR are a
   different problem (secondary: they render to the mirror window, with
   caveats) and would need their own investigation.
4. Network path between the two machines: wired, Wi-Fi, same LAN? Is the
   Windows box on Tailscale? (The Tailscale name would make a stable host
   address for Moonlight; direct LAN is lower latency.)
5. Does multiplayer on integrity-checked servers matter? It decides whether
   EUFD, KU and TEDAC export are on the table at all (see Milestone 4).
6. Pilot seat only, or CPG too?
7. The authoritative viewport names, from the install itself:

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
  from its README). The driver is a plain IddCx monitor, so DCS sees it at
  launch like any other display and no launch ordering is needed.
- Sunshine on Windows. `output_name` set to the virtual display's
  `device_id` from `%ProgramFiles%\Sunshine\tools\dxgi-info.exe`;
  `dd_configuration_option = ensure_active` so the stream re-enables the
  display if Windows dropped it; `dd_resolution_option = auto` so the display
  is switched to whatever Moonlight asks for (all verified from Sunshine's
  configuration.md). Audio stays on the host (`--audio-on-host` on the
  client).
- moonlight-qt on NixOS. CLI verified from its source:
  `moonlight stream <host> Desktop --display-mode fullscreen
  --resolution WxH --fps 60 --bitrate N --audio-on-host --quit-after`.
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
  the EUFD and KU, which are text displays -- see Milestone 4.

## Milestone 1: the Thinkpad shows a Windows virtual monitor

Tracer bullet. No DCS involved. Demo: a stopwatch window dragged onto the
virtual monitor appears on the Thinkpad; a photo of both screens gives the
glass-to-glass latency.

1. Windows: install the Virtual Display Driver with one mode at the Thinkpad
   panel's resolution and refresh rate; arrange it right of the main monitor.
   Check: a window dragged off the right edge disappears into it. Commit
   `windows/vdd_settings.xml`.
2. Windows: install Sunshine; set `output_name`, `dd_configuration_option`,
   `dd_resolution_option`, encoder; pair the Thinkpad. Commit the settings
   as `windows/sunshine.conf` (only the keys we set, never the state file
   with pairing secrets).
3. NixOS: `nixos/moonlight.nix`, imported by kuusi in
   `nixos/systems/flake.nix`. Adds `pkgs.moonlight-qt` and a `dcs-mfd` script
   wrapping the `moonlight stream` invocation with the host, resolution,
   fps and bitrate baked in. Check: the demo above, plus the latency number
   recorded here.

## Milestone 2: Apache MFDs on the Thinkpad

Demo: in the AH-64D in a free-flight mission, both MFDs appear on the
Thinkpad, the main view is unchanged, menus stay on the main screen, and the
fps hit is measured.

1. `windows/MonitorSetup/ThinkpadMFD.lua` with Center, LEFT_MFCD, RIGHT_MFCD,
   UIMainView, GU_MAIN_VIEWPORT, with a copy step into Saved Games (by hand,
   or `windows/install.ps1` once there are three files to copy). DCS
   options: resolution = main + virtual, borderless, monitors = this preset.
   Check: the viewports land where expected; fix `x` if the window origin
   assumption was wrong.
2. Layout. MPD screens are near-square; on a 16:9 or 16:10 panel two
   side-by-side squares of height `panel_width / 2` leave a strip free above
   or below for Milestone 4. Record the chosen rectangles and why.
3. Tune and measure: DCS fps with and without the export, Sunshine fps and
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

## Milestone 4 (stretch): EUFD and KU beside the MFDs

Two routes, decided by input 5 above.

- Integrity-check-breaking route: add `try_find_assigned_viewport` calls in
  the module's EUFD/KU init Lua and define the viewports in the free strip.
  Single-player only; must be redone after every DCS update.
- Integrity-safe route: Export.lua sends `list_indication` text for the EUFD
  and KU over UDP to kuusi, and a small Haskell program renders them in a
  window beside a windowed (not fullscreen) Moonlight. The Thinkpad becomes a
  composite of streamed pixels and locally rendered text. Speculative until
  someone checks what `list_indication` actually returns for those two
  devices.

## Milestone 5 (stretch): touch the MFD bezels

If the Thinkpad panel is a touchscreen: an overlay of bezel buttons around
each MFD on the Thinkpad sends button presses to DCS-BIOS's UDP command port
on the Windows machine. Clicks forwarded by Moonlight do nothing, since
exported viewports are not clickable in DCS, so this needs DCS-BIOS (or a
keybind emulator) regardless. Not planned until Milestones 1-3 are done.
