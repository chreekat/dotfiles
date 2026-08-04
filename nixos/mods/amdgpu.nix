{
  boot.kernelParams = [
    "radeon.cik_support=0"
    "amdgpu.cik_support=1"
    "amdgpu.dc=1"
    "amdgpu.ppfeaturemask=0xffffffff"
  ];

  services.xserver = {
    videoDrivers = ["amdgpu"];

    deviceSection = ''
      Option "TearFree" "true"
      Option "DRI" "3"
      # HW cursor plane wedges after resume/mode events and stops painting the
      # pointer; software cursor draws it into the framebuffer so it can't vanish.
      Option "SWcursor" "true"
    '';

    screenSection = ''
      DefaultDepth 24
    '';
  };
  hardware.enableRedistributableFirmware = true;
}
