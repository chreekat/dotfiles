# Change this if you want to deploy.
# shellcheck disable=SC2190
old=(
    "puny"
    /nix/store/j3n6grihj6h1f2708m11lwr9w825j76x-nixos-system-puny-26.05.20261105.c56a87d
    "honk"
    /nix/store/wibvajvq8sq3n54iliv0jj1l4b2y32dd-nixos-system-honk-26.05.20261105.c56a87d
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
