" logtxt.vim - syntax for your log format
if exists("b:current_syntax")
  finish
endif

syntax match LogtxtDate /^##.*/
syntax match LogtxtDate /^#\+ Week \d\+/
syntax match LogtxtDate /^--.*/
syntax match LogtxtDate /^\d\{4}-\d\{2}-\d\{2}/
syntax match LogtxtTime /^\d\{2}:\d\{2}:/

syntax match LogtxtTimedot /^\S.\{-}\ze  /

highlight default link LogtxtDate Constant
highlight default link LogtxtTimedot Statement
highlight default link LogtxtTime Constant

let b:current_syntax = "logtxt"
