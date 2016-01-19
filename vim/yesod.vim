function! s:touchTemplateUser(nam)
    call system("git grep -l '\"" . a:nam . "\"'| xargs touch")
    GhciReload
endfunc

set tags+=tags.yesod

"
" Yesod Repl
"

" 1. Setup the repl pane. TODO: Start ghci?

command! -nargs=*
    \ GhciRepl
    \ let g:ghci_repl_cmd="<args>"
    \ |let g:ghci_repl=1

command! GhciRestart
    \ call s:ghciCmd("C-d")
    \| call s:ghciCmd("stack\\ ghci\\ --no-load")

command! GhciReload call s:ghciReload()
command! GhciClear  call s:ghciClear()

" 2. Once set up, I want to reload and run a command on each write.
let s:ghciPane = 2

function! s:ghciSendKeys(str)
    call system("tmux send-keys -t " . s:ghciPane . " " . a:str)
endfunc

function! s:ghciCmd(cmd)
    call s:ghciSendKeys("q C-u")
    call s:ghciSendKeys(a:cmd . ' Enter')
endfunc

function! s:ghciFreshCmd(cmd)
    call s:ghciClear()
    call s:ghciCmd(a:cmd)
    call s:ghciRunReplCmd()
endfunc

function! s:ghciClear()
    call s:ghciSendKeys("C-c")
    " Sometimes the C-l seems to get swallowed...
    sleep 20m
    call s:ghciSendKeys("C-l")
    call system("tmux clear-history -t " . s:ghciPane)
endfunc

function! s:ghciRunReplCmd()
    if exists('g:ghci_repl_cmd')
        call s:ghciCmd(g:ghci_repl_cmd)
    endif
endfunc

function! s:ghciReload()
    if exists('g:ghci_repl')
        call s:ghciClear()
        call s:ghciCmd(":r")
        call s:ghciRunReplCmd()
    endif
endfunction

function! s:ghciReloadWith(cmd)
    if exists('g:ghci_repl')
        call s:ghciSendKeys("C-c")
        " Sometimes the C-l seems to get swallowed...
        sleep 20m
        call s:ghciSendKeys("C-l")
        call system("tmux clear-history -t " . s:ghciPane)
        call s:ghciCmd(":r")
        call s:ghciCmd(a:cmd)
    endif
endfunc

function! s:loadGhciErrors()
    if !exists("b:ghciErrFile")
        let b:ghciErrFile = tempname()
    endif

    return s:loadErrors(s:ghciPane)
endfunction

" 3. If there are boo boos, I want to be able to load them up.

nnoremap <leader>re :call <sid>loadGhciErrors()<cr>

function! s:loadErrors(pane)
        let ef = b:ghciErrFile
        call system("tmux capture-pane -J -S -32000 -t " . a:pane)
        call system("tmux save-buffer " . ef)
        call system("tmux delete-buffer")
        call system("sed -i -e 's#\\s*$##g' " . ef)
        exec "cf " . ef
endfunc

"
" Yesod template jumping.
"

function! s:templateJumping()
    nnoremap <buffer> <leader>th :exec ":e templates/" . expand("<cfile>") . ".hamlet"<cr>
    nnoremap <buffer> <leader>tc :exec ":e templates/" . expand("<cfile>") . ".cassius"<cr>
    nnoremap <buffer> <leader>tj :exec ":e templates/" . expand("<cfile>") . ".julius"<cr>
endfunc

"
" Execute!
"

GhciRepl main

augroup yesod
    au!
    au BufWritePost */templates/*.hamlet call s:touchTemplateUser(expand('%:s?templates/??:r'))
    au BufWritePost */messages/*  call system("touch src/Foundation.hs")|GhciReload
    au BufWritePost config/routes  call system("touch src/Foundation.hs")|GhciReload
    au BufWritePost */messages/* call system("~/lib/message-tags")
    au BufWritePost *.hs GhciReload
    au BufRead,BufNewFile *.hs call s:templateJumping()
aug END
