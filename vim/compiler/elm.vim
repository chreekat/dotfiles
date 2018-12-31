let current_compiler = "elm"

" ORDER MATTERS!!

" I <3 vim

let efmlist = []

" NAMING ERROR
let efmlist += ["%E%>-- NAMING ERROR ----------------------------------------- %f"]
let efmlist += ["%C"]
let efmlist += ["%C%l|%.%#"]
let efmlist += ["%C  %p^%.%#"]
let efmlist += ["%C%m"]

exec "CompilerSet efm=" . escape(join(efmlist, ","), ' |')
