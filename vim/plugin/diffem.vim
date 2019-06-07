" Under development
"

" This plugin provides a command that enables a git diff mode. A new arglist is
" created containing all files with diffs, and additional commands are enabled
" for moving backwards and forwards in the arglist while diffing the files
" against the target commit.

command! -nargs=? DiffEm call s:diffEm(<q-args>)

function! s:diffEm(commit)
    function! s:godiff() closure
        exec "Gdiff" a:commit
    endfunc
    silent let changedFiles = systemlist("git diff --name-only --relative " . a:commit)
    if v:shell_error == 0
        command! DiffOff call s:diffOff()
        command! Godiff call s:godiff()
        command! Gonext only|next|Godiff
        command! Goprev only|prev|Godiff

        arglocal
        %argdelete
        for f in changedFiles
            exec "argadd" f
        endfor
        rewind
        Godiff
    endif

endfunction

function! s:diffOff()
    delcommand DiffOff
    delcommand Gonext
    delcommand Goprev
    delcommand Godiff
    argglobal
endfu
