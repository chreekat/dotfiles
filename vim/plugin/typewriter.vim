let s:nnops = [ 'd', 'c', 's', 'x', 'v', 'S' ]
let s:inops = [ '<bs>', '<c-t>', '<c-d>', '<c-o>' ]

com! -nargs=0 Typewriter
    \ if !exists("b:typewriter")
    \ | call Typewriter()
    \ | else | call TypewriterReset()
    \ | endif

fu! Typewriter ()
    for c in s:nnops
        exec 'nnoremap <buffer> ' . c . ' <nop>'
    endfor
    for c in s:inops
        exec 'inoremap <buffer> ' . c . ' <nop>'
    endfor
    let b:typewriter = 1
    Goyo 80
endfu

fu! TypewriterReset ()
    Goyo!
    for c in s:nnops
        exec 'nunmap <buffer> ' . c
    endfor
    for c in s:inops
        exec 'iunmap <buffer> ' . c
    endfor
    unlet b:typewriter
endfu
