if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "org"

syn match orgHeading "^\*\+ .*"

augroup AfterOrg
    au!
    au ColorScheme * hi link orgHeading Label
    au ColorScheme * hi Folded NONE
    au ColorScheme * hi link Folded Label
augroup END

hi link orgHeading Label
hi Folded NONE
hi link Folded Label
