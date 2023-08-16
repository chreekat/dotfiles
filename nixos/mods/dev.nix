{ ...}: {
  environment.systemPackages = [
    (import (fetchTarball "https://install.devenv.sh/latest")).default
  ];
}
