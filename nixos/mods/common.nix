{
  boot.kernelModules = [ "tcp_bbr" ];
  net.ipv4.tcp_congestion_control = "bbr";
}
