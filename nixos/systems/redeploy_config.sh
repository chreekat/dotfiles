# Change this if you want to deploy.
# shellcheck disable=SC2190
old=(
    "puny"
    /nix/store/rz805lhpygbmrg88klph3rk834knhk3p-nixos-system-puny-25.05.20251010.7e297dd
    "honk"
    /nix/store/4jx52xnaqwhhkh83737yg9j1vk1rrc30-nixos-system-honk-25.05.20251026.78e34d1
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
    ["puny"]="--use-remote-sudo"
    ["honk"]="--use-remote-sudo"
    ["kuusi"]=""
);

redeploy_prehook () {
    rm ../mods/syncthing.nix
    gpg ../mods/syncthing.nix.asc
    trap '> ../mods/syncthing.nix' EXIT
}
