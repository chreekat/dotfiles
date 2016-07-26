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

nnoremap <buffer> <F8> :lgrep "waiting for:" %<cr>
nnoremap <buffer> <F7> :lgrep "next action:" %<cr>

" This is needed for all prose files, really, but we'll start with just
" this one.
setl smartcase

vmap <buffer> <leader>m d'i>p<c-o><c-o>
