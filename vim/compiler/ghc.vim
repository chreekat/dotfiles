let current_compiler = "ghc"

" I <3 vim
CompilerSet efm =
    \%f:%l:%c:\ %trror:\ %m,
    \%f:%l:%c:\ %tarning:\ %m,
    \%E%f:%l:%c:\ error:,
    \%W%f:%l:%c:\ warning:,
    \%Z\ \ \ \ •\ %m
