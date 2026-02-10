# Change this if you want to deploy.
# shellcheck disable=SC2190
old=(
    "puny"
    /nix/store/6f1pnn4kgnnydvyj00xvr14gh8qjyr7f-nixos-system-puny-25.11.20260209.2db38e0
    "honk"
    /nix/store/ld2cjn7mc2pd3ciq9rwxqig0qvhqgf25-nixos-system-honk-25.11.20260113.2c3e5ec
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
