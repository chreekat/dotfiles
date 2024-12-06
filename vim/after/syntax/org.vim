if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "org"

syn match orgHeading "^\*\+ .*"

augroup AfterOrg
    au!
    au ColorScheme * hi link orgHeading StatusLine
augroup END
