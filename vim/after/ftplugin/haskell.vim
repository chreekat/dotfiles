try
    compiler cabal
catch /E666/
endtry

noremap <leader>rR :exec "VimuxRunCommand \":l " . expand("%:.") . "\""<cr>
noremap <leader>rr :exec "VimuxRunCommand \":r\""<cr>
noremap <leader>rl :VimuxRunLastCommand<cr>
noremap <leader>rg :exec "VimuxRunCommand \"ghcid\""<cr>
noremap <leader>rb :VimuxScrollUpInspect<cr>
noremap <leader>rf :VimuxScrollDownInspect<cr>
noremap <leader>rq :VimuxCloseRunner<cr>


" Text-object representing a top-level function. Set for operater, visual,
" and select modes.
noremap af ?^\k<cr>o/<cr>{
nunmap af

" Add quote (prime) to list of keyword chars
set isk+='

set efm=%f:%l:%c:\ %t%*[^:]:\ %m,%W%f:%l:%c:\ Warning:,%C\ \ \ \ %m,%Z,%E%f:%l:%c:,%E%f:%l:%c:\ %m

noremap -ge ?^module<cr>
noremap -gi ?^import<cr>

function! s:updateTags(f)
    if filewritable("tags")
        call system("fast-tags " . a:f)
    endif
endfunc

augroup afterHaskell
    au!
    au BufWritePost <buffer> call s:updateTags(expand('%'))
aug END
