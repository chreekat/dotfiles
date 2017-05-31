setlocal iskeyword+='
setlocal includeexpr=(substitute(v:fname,'\\.','/','g').'.hs')

augroup after_haskell
    autocmd!
    autocmd BufWrite <buffer> call s:update_tags(expand("<afile>"))
augroup END

function! s:update_tags(f)
    if filewritable("tags")
        call system("fast-tags " . a:f)
    endif
endfunc
