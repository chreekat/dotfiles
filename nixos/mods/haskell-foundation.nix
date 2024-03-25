# Things needed for HF work
{ pkgs, ... }: {
# TODO 2024-01-04
# {
#   systemd.network.netdevs.wg0 = {
#     netdevConfig = {
#       Kind = "wireguard";
#       Name = "haskell.org";
#     };
#     wireguardConfig = {
#     };
#     wireguardPeers = [
#       {
#         publicKey = "kOObwlKAASsREeWieEvnwNUMbElqCWYwupHs34/G/ns=";
#         allowedIPs = [ "" ];
#     ];
#   };
# [Interface]
# Address = 172.16.0.8/24
# PrivateKey = lol
# MTU = 1400
#
# [Peer]
# PublicKey = kOObwlKAASsREeWieEvnwNUMbElqCWYwupHs34/G/ns=
# AllowedIPs = 172.16.0.0/16
# Endpoint = ci-vpn.haskell.org:51820

}
