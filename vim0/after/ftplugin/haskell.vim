"
" General Haskell Stuff
"

compiler ghc

" Add quote (prime) to list of keyword chars
setl isk+='
setl tags+=codex.tags,*/codex.tags
setl keywordprg=xdg-open\ http://localhost:5555/?hoogle=\

" Includes
let &l:include = "^import\\s*\\%(qualified\\)\\?\\s*"
let &l:includeexpr = "substitute(v:fname, '\\.', '/', 'g') . '.hs'"
"
" Jump to typical file locations (note: should probably make these markers)
"    exports
nnoremap <buffer> <leader>ge ?^module<cr>
"    imports
nnoremap <buffer> <leader>gi ?^import<cr>

" Jumping to TLD ("names")
noremap <buffer> <leader>n :call search('^\w', 'W')<cr>
noremap <buffer> <leader>N :call search('^\w', 'Wb')<cr>

"
" Autocommand plumbing
"

function! s:updateTags(f)
    if filewritable("tags")
        call system("fast-tags " . a:f)
    endif
endfunc

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
