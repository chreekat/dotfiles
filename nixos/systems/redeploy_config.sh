# Change this if you want to deploy.
# shellcheck disable=SC2190
old=(
    "puny"
    /nix/store/wl1ribl7dfsxplmx03a8yhdrwfdmhv3g-nixos-system-puny-26.05.20260817.0dd31db
    "honk"
    /nix/store/yljm183gn2dr3sflgayifdyzv0kpk8bz-nixos-system-honk-26.05.20260817.0dd31db
    "kuusi"
    foo
)

# shellcheck disable=SC2190
target=(
    "puny" puny.chreekat.net
    "honk" 95.216.0.246
    "kuusi" kuusi.bryanthomasrichter.gmail.com.beta.tailscale.net
)

nixos_rebuild_args=(
    ["puny"]="--sudo"
    ["honk"]="--sudo"
    ["kuusi"]=""
);

redeploy_prehook () {
    rm ../mods/syncthing.nix
    gpg ../mods/syncthing.nix.asc
    trap '> ../mods/syncthing.nix' EXIT
}
