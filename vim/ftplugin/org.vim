" Start a fold on orgmode headers. Number of '*' is the foldlevel.
function OrgFold()
    let l:line = getline(v:lnum)
    let l:stars = matchstr(l:line, '^\*\+')
    if l:stars != ""
        let l:level = len(l:stars)
        return '>' . l:level
    else
        return '='
    endif
endfunc

setl fde=OrgFold()
setl fdm=expr
