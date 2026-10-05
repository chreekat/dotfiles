-- Installed as Saved Games\DCS\Config\MonitorSetup\ThinkpadMPD.lua and picked
-- under Options > System > Monitors, with resolution 5360x1440, borderless.
-- Columns 3440 and up are the virtual monitor that kuusi shows.
_  = function(p) return p end
name = _('ThinkpadMPD')
description = 'Main monitor, with the MPDs on the Thinkpad'

Viewports =
{
  Center =
  {
    x = 0,
    y = 0,
    width = 3440,
    height = 1440,
    viewDx = 0,
    viewDy = 0,
    aspect = 3440 / 1440,
  }
}

LEFT_MFCD =
{
  x = 3440,
  y = 0,
  width = 960,
  height = 960,
}

RIGHT_MFCD =
{
  x = 4400,
  y = 0,
  width = 960,
  height = 960,
}

UIMainView = Viewports.Center
GU_MAIN_VIEWPORT = Viewports.Center
