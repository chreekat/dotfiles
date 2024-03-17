# ls -al /dev/disk/by-id
# lrwxrwxrwx 1 root root  13 Mar 14 10:12 nvme-eui.0025388491b7f34b -> ../../nvme1n1
# lrwxrwxrwx 1 root root  13 Mar 14 10:12 nvme-eui.0025388491b7f373 -> ../../nvme0n1
let
  nmve0n1_id = "/dev/disk/by-id/nvme-eui.0025388491b7f373";
  nmve1n1_id = "/dev/disk/by-id/nvme-eui.0025388491b7f34b";
  poolName = "zroot";
  mkDisk = { device, boot ? false }: {
    inherit device;
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        boot = {
          size = "500M";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = if boot then "/boot" else null;
          };
        };
        swap = {
          size = "4G";
          content = {
            type = "swap";
            randomEncryption = true;
          };
        };
        root = {
          size = "100%";
          content = {
            type = "zfs";
            pool = poolName;
          };
        };
      };
    };
  };
in
{
  disko.devices = {
    disk = {
      nmve0n1 = mkDisk { device = nmve0n1_id; boot = true; };
      nmve1n1 = mkDisk { device = nmve1n1_id; };
    };
    zpool = {
      "${poolName}" = {
        type = "zpool";
        mode = "mirror";
        rootFsOptions = {
          compression = "lz4";
        };
        mountpoint = "/";
        # TODO: datasets
      };
    };
  };
# I don't care about FDE since everything in Matrix is shared to other servers,
# anyway.
#
# Partition table: GPT
#
# Partitions: boot, swap, one big mirror pool.
#
# RAID: mirror for /boot
#
# Filesystems: root, postgres, reserved.
}
