# Change this if you want to deploy.
# shellcheck disable=SC2190
old=(
    "puny"
    /nix/store/r9182fh2z6svgq9ghbad7w7pn14gm3n5-nixos-system-puny-26.05.20261102.7dcdd87
    "honk"
    /nix/store/n75nkkdmlccxp0k2kgmrvnf1f4sf5h63-nixos-system-honk-26.05.20261102.7dcdd87
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
