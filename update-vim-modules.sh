#!/usr/bin/env bash

set -Eeuo pipefail

##
git submodule update --remote
vim +"helptags ALL" +q
