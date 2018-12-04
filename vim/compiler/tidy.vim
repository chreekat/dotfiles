let current_compiler = "tidy"

let efmlist = []

let efmlist += ["line %l column %c - %trror: %m"]
let efmlist += ["line %l column %c - %tarning: %m"]

exec "CompilerSet efm=" . escape(join(efmlist, ","), ' ')

CompilerSet mp=tidy\ -qe\ %
