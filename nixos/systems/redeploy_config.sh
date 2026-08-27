# Change this if you want to deploy.
# shellcheck disable=SC2190
old=(
    "puny"
    /nix/store/wl1ribl7dfsxplmx03a8yhdrwfdmhv3g-nixos-system-puny-26.05.20260817.0dd31db
    "honk"
    /nix/store/yljm183gn2dr3sflgayifdyzv0kpk8bz-nixos-system-honk-26.05.20260817.0dd31db
    "kuusi"
    /nix/store/vhyi09z8rzm4xgbaqizdp0yzsaknkf7w-nixos-system-kuusi-26.05.20260823.a3b9886
)

# shellcheck disable=SC2190
target=(
    "puny" puny.chreekat.net
    "honk" 95.216.0.246
    # sshd only listens on the tailscale address; see listenAddresses in
    # kuusi/configuration.nix.
    "kuusi" kuusi.tail062b9.ts.net
)

nixos_rebuild_args=(
    ["puny"]="--sudo"
    ["honk"]="--sudo"
    ["kuusi"]="--sudo"
);

redeploy_prehook () {
    # Only puny imports the encrypted syncthing config.
    [ "$1" = "puny" ] || return 0
    rm ../mods/syncthing.nix
    gpg ../mods/syncthing.nix.asc
    trap '> ../mods/syncthing.nix' EXIT
}
