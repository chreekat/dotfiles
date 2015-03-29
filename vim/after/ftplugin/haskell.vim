try
    compiler cabal
catch /E666/
endtry

noremap <leader>rr :exec "VimuxRunCommand \":l " . expand("%:.") . "\""<cr>
noremap <leader>rl :VimuxRunLastCommand<cr>

" Text-object representing a top-level function. Set for operater, visual,
" and select modes.
noremap af ?^\k<cr>o/<cr>{
nunmap af
