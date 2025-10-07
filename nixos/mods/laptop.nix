{
  services.tlp.enable = true;
  # Keep charge less than max when plugged in. Saves battery life.
  services.tlp.settings = {
    START_CHARGE_THRESH_BAT0 = 90;
    STOP_CHARGE_THRESH_BAT0 = 92;
  };

  # Prevent disaster; be useful
  services.logind.powerKey = "suspend";

}
