" Searched for something? Recorded a macro that did something to it? Use
" :AndSoOn to keep going through the rest of the file.

function! s:andSoOn(reg)
    while search(@/, "W") > 0
        exec "normal @" . a:reg
    endwhile
endfunction

command! -nargs=1 AndSoOn call s:andSoOn('<args>')
