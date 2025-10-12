alias git-ghc-tags="git ls-files --recurse-submodules -z '*.hs' '*.lhs' | xargs -0 ghc-tags -c"
alias git-fast-tags="git ls-files --recurse-submodules -z '*.hs' '*.lhs' | fast-tags -0 -"
alias hoogle-local-srv="stack hoogle -- server --local -p 5555 -q >/dev/null 2>&1 &"
