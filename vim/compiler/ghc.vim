let current_compiler = "ghc"

" ORDER MATTERS!!

" I <3 vim

let efmlist = []

" Lines to ignore. Vim appears to be treating spaces as filename elements,
" attempting to open "In file included from src/Databrary/Store/avFrame.h" as a
" file.
""
let efmlist += ["%-GIn file included from%.%#"]

" GHC has multiple ways of specifying error location.
""

let locSpecifier = []

" Up to some ghc version, only the start column was specified
let locSpecifier += ["%f:%l:%c: "]

" Later, ranges were specified, which we must ignore
let locSpecifier += ["%f:%l:%c-%*\\\\d: "]

" Ranges can also be 2-d :o
let locSpecifier += ["%f:(%l\\\\,%c)-(%*\\\\d\\\\,%*\\\\d): "]

for loc in locSpecifier
    " One-line errors
    ""
    let efmlist += [loc . "%trror: %m"]
    let efmlist += [loc . "%tarning: %m"]

    " Multiline errors.
    "
    " Just pull the second line (view the rest of the error with cl! and
    " friends).
    ""
    let efmlist += ["%E" . loc . "fatal:"]
    let efmlist += ["%E" . loc . "error:"]
    let efmlist += ["%W" . loc . "warning:"]
endfor

" >= 8.2
let efmlist += ["%Z    • %m"]
" < 8.2
let efmlist += ["%Z    %m"]
let efmlist += ["%Z    * %m"]

exec "CompilerSet efm=" . escape(join(efmlist, ","), ' ')
