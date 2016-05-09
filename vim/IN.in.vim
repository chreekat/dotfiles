command! -buffer GoMark
    \|let gomark = getchar()
    \|try
        \|exec "normal '".nr2char(gomark)."zx"
    \|catch /./
    \|endtry
nnoremap <buffer> <silent> <leader>g :GoMark<cr>
nmap <buffer> -np -gpo  proj<tab>

" Make sure the t and n registers still work..
let @t = "dd30jp$khhh:ID'tj"
let @n = "^vg_y:let @/ = @0n"
