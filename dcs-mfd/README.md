# dcs-mfd

Turn the NixOS Thinkpad into a secondary display for the AH-64D's MFDs while
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
- DCS's monitor-setup Lua places the MFD viewports in that monitor's region.
- Sunshine captures only that monitor and streams it with the GPU's encoder.
- Moonlight on the Thinkpad shows the stream fullscreen.

See [PLAN.md](PLAN.md) for the milestones and the open questions.

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
