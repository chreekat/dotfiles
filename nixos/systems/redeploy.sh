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
# shellcheck disable=SC2190
old=(
    "puny"
    /nix/store/k1p98as4nxnxq118d96vfkcwx6pxmyqq-nixos-system-puny-24.11.20250310.ebe2788
    "honk"
    /nix/store/dpc33c169fid56mb4cimy9pw2a7rm9a7-nixos-system-honk-24.11.20250310.ebe2788
    "kuusi"
    foo
)

# shellcheck disable=SC2190
target=(
    "puny" puny.chreekat.net
    "honk" 95.216.0.246
    "kuusi" kuusi.bryanthomasrichter.gmail.com.beta.tailscale.net
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
    if [[ "${2:-}" = '-f' ]]; then
        >&2 echo
        >&2 echo "*** Forcing a redeploy of a NEW configuration for $1."
        >&2 echo
        rebuild "$1" switch --target-host "${target["$1"]}"
        >&2 echo
        >&2 echo "*** New result for $1: $(readlink result)"
    else
        >&2 echo
        >&2 echo "*** This is a NEW configuration. Edit $0 if you're satisfied with it."

        if [ -t 0 ]; then
            read -rp "Show diff? [y/N] " yn
            if [[ $yn = [yY] ]]; then
                nix-diff --color always "${old["$1"]}" "$new" | less && true
            fi
        fi
        >&2 echo
        >&2 echo "*** New result for $1: $(readlink result)"
        exit 1
    fi
elif [[ $new = "$current" ]]; then
    echo "*** No change to system. Not deploying."
else
    rebuild "$1" switch --target-host "${target["$1"]}"
fi
