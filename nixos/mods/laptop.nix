{
  services.tlp.enable = true;
  # Keep charge betwene 80-82% when plugged in. Saves battery life.
  services.tlp.settings = {
    START_CHARGE_THRESH_BAT0 = 80;
    STOP_CHARGE_THRESH_BAT0 = 82;
  };
}
