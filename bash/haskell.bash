alias git-fast-tags="git ls-tree -r HEAD --name-only | grep -E '\.l?hs$' | xargs fast-tags -v"
alias hoogle-local-srv="stack hoogle -- server --local -p 5555 -q >/dev/null 2>&1 &"
