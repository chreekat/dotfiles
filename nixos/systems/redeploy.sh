#!/usr/bin/env bash

set -Eeuo pipefail

# Usage:
#        ./redeploy.sh matrix-builder

# Redeploys a server. Enforces that the last-deployed state is recorded in this
# file. This might be more pain than it's worth, but the idea was to keep track
# of deployment conflicts since there are multiple people who do them. Better
# ideas welcome.

# Since this deploys to a remote server, /etc/nixos is unused on that server.
# Consider placing placeholder-for-redeploy.sh.nix in
# /etc/nixos/configuration.nix to avoid calamity.

declare -A old target

# Change this if you want to deploy.
old=(
    "puny"
    /nix/store/xa3saa51hdkrykr6kfpgkmvfwsbm1vmq-nixos-system-puny-23.05.20231029.3e10c80
    "honk"
    /nix/store/kk2y337b37nbpnz03hk0rkmwclsragal-nixos-system-honk-23.05.20240103.70bdade
)

target=(
    "puny" puny
    "honk" 95.216.0.246
)

rebuild () {
    set -Eeuo pipefail
    system="$1"
    shift
    nixos-rebuild --use-remote-sudo --use-substitutes --flake ".#${system}" "$@"
}

rm ../mods/syncthing.nix
gpg ../mods/syncthing.nix.asc
trap '> ../mods/syncthing.nix' EXIT

rebuild "$1" build

new="$(readlink result)"
current="$(ssh "${target["$1"]}" readlink /run/current-system)"

if [[ ${old["$1"]} != "$new" ]]; then
    >&2 echo
    >&2 echo "*** This is a NEW configuration. Edit $0 if you're satisfied with it."
    >&2 echo
    >&2 echo "*** New result for $1: $(readlink result)"
    exit 1
elif [[ $new = "$current" ]]; then
    echo "*** No change to system. Not deploying."
else
    rebuild "$1" switch --target-host "${target["$1"]}"
fi
