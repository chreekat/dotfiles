let current_compiler = "ghc"

" I <3 vim
CompilerSet efm =
    \%C\(bound\ at,
    \%C\ \ \ \ %m,
    \%-G\\\\s%#,
    \%E%f:%l:%c:\ Error:\ %m,
    \%E%f:%l:%c:\ Error:,
    \%W%f:%l:%c:\ Warning:\ %m,
    \%W%f:%l:%c:\ Warning:,
    \%E%f:%l:%c:\ %m,
    \%E%f:%l:%c:,
    \%E%f:%l:\ %m,
    \%-G%.%#
