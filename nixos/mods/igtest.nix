{
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    virtualHosts."igtest.chreekat.net" = {
      enableACME = true;
      forceSSL = true;
      locations."/".proxyPass = "http://localhost:8388";
    };
  };
}
