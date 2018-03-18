let current_compiler = "ghc"

" ORDER MATTERS!!

" I <3 vim

let efmlist = []

" - One-line errors
""
let efmlist += ["%f:%l:%c: %trror: %m"]
let efmlist += ["%f:%l:%c: %tarning: %m"]

" - Multiline errors.
"
" Just pull the second line (view the rest of the error with cl! and friends).
""

let efmlist += ["%E%f:%l:%c: error:"]
let efmlist += ["%W%f:%l:%c: warning:"]
" >= 8.2
let efmlist += ["%Z    • %m"]
" < 8.2
let efmlist += ["%Z    %m"]
let efmlist += ["%Z    * %m"]

exec "CompilerSet efm=" . escape(join(efmlist, ","), ' ')
