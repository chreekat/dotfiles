command! -buffer GoMark
    \|let gomark = getchar()
    \|try
        \|exec "normal '".nr2char(gomark)."zx"
    \|catch /./
    \|endtry
nnoremap <buffer> <silent> <leader>g :GoMark<cr>
nmap <buffer> -ni -gio  -<space>
nmap <buffer> -np -gpo  proj<tab>
