"
" General Haskell Stuff
"

" Add quote (prime) to list of keyword chars
compiler ghc

setl isk+='
setl tags+=codex.tags,*/codex.tags

nnoremap <buffer> <leader>ge ?^module<cr>
nnoremap <buffer> <leader>gi ?^import<cr>

function! s:updateTags(f)
    if filewritable("tags")
        call system("fast-tags " . a:f)
    endif
endfunc

"
" Jumping to TLD ("names")
"

noremap <buffer> <leader>n :call search('^\w', 'W')<cr>
noremap <buffer> <leader>N :call search('^\w', 'Wb')<cr>


"
" Autocommand plumbing
"

augroup afterHaskell
    au! * <buffer>
    au BufWritePost <buffer> call s:updateTags(expand('%'))
aug END

"
" Add alignment for 't'ype annotations
"
if !exists("g:easy_align_delimiters")
    let g:easy_align_delimiters = {}
endif
let g:easy_align_delimiters["t"] = { 'pattern': '->\|=>\|::' }
