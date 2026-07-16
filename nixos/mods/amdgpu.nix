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
    '';

    screenSection = ''
      DefaultDepth 24
    '';
  };
  hardware.enableRedistributableFirmware = true;
}
