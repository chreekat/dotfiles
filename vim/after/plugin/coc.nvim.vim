xmap <leader>ca  <Plug>(coc-codeaction-selected)
nmap <leader>cR <Plug>(coc-references-used)
nmap <leader>crn <Plug>(coc-rename)

" Just eat the command because we don't actually use it
command! -nargs=1 CocHover call CocActionAsync('doHover')
set keywordprg=:CocHover

" Let's do this manually rather than keep getting caught waiting for HLS to
" finish spinning up and hardlocking vim.
"set tagfunc=CocTagFunc
