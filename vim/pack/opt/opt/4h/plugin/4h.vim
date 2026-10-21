" Format Haskell buffers with 4h on save.
" Opt-in per session: :packadd 4h
" Also provides :Fourh to format the current buffer without writing.

if exists('g:loaded_4h')
    finish
endif
let g:loaded_4h = 1

function! s:format() abort
    " --stdin-input-file anchors .cabal discovery; pathless buffers get none
    let l:cmd = empty(expand('%:p'))
        \ ? '4h --no-cabal'
        \ : '4h --stdin-input-file ' .. shellescape(expand('%:p'))
    let l:out = system(l:cmd, join(getline(1, '$'), "\n") .. "\n")
    if v:shell_error
        echohl WarningMsg | echomsg '4h failed (buffer unchanged)' | echohl None
        return
    endif
    let l:new = split(l:out, "\n")
    if l:new !=# getline(1, '$')
        let l:view = winsaveview()
        call setline(1, l:new)
        if line('$') > len(l:new)
            silent execute len(l:new) + 1 .. ',$delete _'
        endif
        call winrestview(l:view)
    endif
endfunction

command! Fourh call s:format()

augroup fourh
    autocmd!
    autocmd BufWritePre * if &filetype ==# 'haskell' | call s:format() | endif
augroup END
