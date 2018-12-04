setlocal iskeyword+='
setlocal includeexpr=(substitute(v:fname,'\\.','/','g').'.hs')
setlocal equalprg=brittany

compiler ghc

syn sync fromstart

augroup after_haskell
    autocmd!
    autocmd BufWrite <buffer> call s:update_tags(expand("<afile>"))
augroup END

function! s:update_tags(f)
    if filewritable("tags")
        call system("fast-tags " . a:f)
    endif
endfunc

function! s:choose_compiler()
    let choices = { 'stack': 'stack build'
                 \, '&pedantic stack': 'stack build --pedantic'
                 \, 'pedantic stack &test': 'stack test --fast --pedantic'
                 \, 'hlint': 'hlint\ %'
                 \}
    let choice = confirm("Which compiler?", join(keys(choices), "\n"))
    let compiler = choices[keys(choices)[choice - 1]]
    let &mp = l:compiler
endfunction
nnoremap <buffer> <F5> :call <SID>choose_compiler()<cr>

command! -buffer Hlint cexpr system("hlint ".expand("%"))
command! -buffer -range Dollar sil <line1>|sil exe "normal $F$caw("|sil <line2>|sil normal A)
command! -buffer -range=% Hindent <line1>,<line2>!hindent --indent-size 4
