xmap <leader>ca  <Plug>(coc-codeaction-selected)
nmap <leader>cR <Plug>(coc-references)
nmap <leader>crn <Plug>(coc-rename)

" Just eat the command because we don't actually use it
command! -nargs=1 CocHover call CocActionAsync('doHover')
set keywordprg=:CocHover

set tagfunc=CocTagFunc
