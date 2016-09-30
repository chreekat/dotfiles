alias git-fast-tags="git ls-tree -r HEAD --name-only | grep -E '*.l?hs$' | xargs fast-tags -v"
alias git-hothasktags="git ls-tree -r HEAD --name-only | grep -E '*.l?hs$' | xargs hothasktags -X TemplateHaskell -X ScopedTypeVariables -X LambdaCase -X RecordWildCards -X MultiWayIf -X CPP -X TypeFamilies -X TupleSections -X QuasiQuotes -O tags"
alias hoogle-local-srv="stack hoogle -- server --local -p 5555 -q"
