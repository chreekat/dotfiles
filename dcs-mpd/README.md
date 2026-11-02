# dcs-mpd

Turn the NixOS Thinkpad into a secondary display for the AH-64D's MPDs while
DCS World runs on the Windows machine.

## How it works

    +---------------- Windows ----------------+        +--- Thinkpad (kuusi) ---+
    | DCS, borderless window spanning:        |        |                        |
    |   [ main monitor ][ virtual monitor ]   |  LAN   |  moonlight-qt,         |
    |     3D view        LEFT_MFCD RIGHT_MFCD |------->|  fullscreen            |
    |                        ^                |        |                        |
    |  Virtual Display Driver | Sunshine      |        |                        |
    |  (adds the monitor)     | (captures it) |        |                        |
    +-----------------------------------------+        +------------------------+

- The Virtual Display Driver adds a monitor to Windows sized to the Thinkpad
  panel.
- DCS's monitor-setup Lua places the MPD viewports in that monitor's region.
- Sunshine captures only that monitor and streams it with the GPU's encoder.
- Moonlight on the Thinkpad shows the stream fullscreen.

See [PLAN.md](PLAN.md) for the milestones and the open questions.

## Flying

Once per machine:

- smilga: `windows\install.ps1` from an elevated PowerShell, as its header
  says. Rerun it after any change to the virtual display (disable/enable in
  VDD Control, a driver reinstall, a move in Settings > Display): the
  display comes back as a new device, and the script re-derives Sunshine's
  `output_name` and regenerates the DCS preset from wherever the monitor
  now sits. Its last line names the DCS resolution to set.
- smilga, DCS: Options > System: that resolution, Fullscreen unchecked,
  Monitors = `ThinkpadMPD`.
- kuusi: `moonlight pair smilga`, PIN into Sunshine's web UI at
  `https://localhost:47990` on smilga.

Every flight:

1. kuusi: `META+F4` (or `dcs-mpd` in a terminal). The stream goes fullscreen
   on its own; `META+Q` toggles it if not.
2. smilga: start DCS as usual. The virtual monitor is always present, so
   order does not matter.
3. Done flying: `Ctrl+Alt+Shift+Q` on kuusi ends the stream and Moonlight
   exits.

If the stream drops mid-flight, `META+F4` again. If Moonlight sits on
"Starting Desktop..." instead, run `dcs-mpd` in a terminal and read its last
lines: a `StreamSegue.qml` TypeError is Moonlight's stale QML cache after an
upgrade (delete `~/.cache/Moonlight Game Streaming Project/Moonlight/qmlcache`);
anything else is Sunshine's side, and `sunshine.log` under
`%ProgramFiles%\Sunshine\config`, with the `Unable to find MAC address`
lines filtered out, says what.

## Layout

- `windows/` -- files copied to the Windows machine: the DCS monitor-setup
  Lua, Sunshine and virtual-display settings, an install script.
- `nixos/` -- the NixOS module for the Thinkpad side, imported from
  `nixos/systems/flake.nix`.

## Moving to its own repo

This directory lives in dotfiles only until the tracer bullet works. When it
moves out: `CLAUDE.md` imports the global rules with `@../claude/CLAUDE.md`,
which must then point at wherever dotfiles is checked out, and
`nixos/systems/flake.nix` must take the module as a flake input instead of a
relative path.
