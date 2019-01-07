let current_compiler = "elm"

" ORDER MATTERS!!

" I <3 vim

let efmlist = []

let efmlist += ["%E%>-- NAMING ERROR -%# %f"]
let efmlist += ["%E%>-- TYPE MISMATCH -%# %f"]
let efmlist += ["%-G==================================== ERRORS ===================================="]
let efmlist += ["%C"]
let efmlist += ["%C%l|%.%#"]
let efmlist += ["%C  %p^%.%#"]
let efmlist += ["%C%m"]

exec "CompilerSet efm=" . escape(join(efmlist, ","), ' |')
