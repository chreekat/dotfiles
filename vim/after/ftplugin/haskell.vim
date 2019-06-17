setlocal iskeyword+='
setlocal includeexpr=(substitute(v:fname,'\\.','/','g').'.hs')

compiler ghc

syn sync fromstart

augroup after_haskell
    autocmd! * <buffer>
    autocmd BufWritePost <buffer> call s:update_tags()
augroup END

function! s:update_tags()
    if filewritable("tags") == 1
        call job_start(
            \["/bin/sh", "-c", "git ls-files *.hs | xargs fast-tags"],
            \{"in_io":"null","out_io":"null","err_io":"null"})
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
