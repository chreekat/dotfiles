let s:nnops = [ 'd', 'c', 's', 'x', 'v', 'S' ]
let s:inops = [ '<bs>', '<c-t>', '<c-d>', '<c-o>', '<c-w>' ]

com! -nargs=0 Typewriter
    \ if !exists("b:typewriter")
    \ | call s:typewriter()
    \ | else | call s:typewriterReset()
    \ | endif

fu! s:typewriter ()
    for c in s:nnops
        exec 'nnoremap <buffer> ' . c . ' <nop>'
    endfor
    for c in s:inops
        exec 'inoremap <buffer> ' . c . ' <nop>'
    endfor
    let b:typewriter = 1
    if !exists("Goyo")
        Goyo 80
    else
        echom "Consider installing Goyo"
    endif
endfu

fu! s:typewriterReset ()
    if exists("Goyo")
        Goyo!
    endif
    for c in s:nnops
        exec 'nunmap <buffer> ' . c
    endfor
    for c in s:inops
        exec 'iunmap <buffer> ' . c
    endfor
    unlet b:typewriter
endfu
