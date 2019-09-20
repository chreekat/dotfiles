" Under development
"

" This plugin provides a command that enables a git diff mode. A new arglist is
" created containing all files with diffs, and additional commands are enabled
" for moving backwards and forwards in the arglist while diffing the files
" against the target rev.

command! -nargs=? DiffEm call s:diffEm(<q-args>)

function! s:diffEm(rev)

    " It seems vim-fugitive doesn't understand 'rev...' syntax. This should
    " maybe go upstream.
    let commit = a:rev
    if match(a:rev, "\.\.\.$") >= 0
        let commit = system(printf("git rev-parse %s | tail -n 1 | sed -e 's/^\^//'", a:rev))
    else
        " Also handle 'rev..' syntax, which we can do just by removing the dots.
        let dots = match(a:rev, "\.\.$")
        if dots >=0
            let commit = strpart(a:rev, 0, dots - 1)
        endif
    endif

    function! s:godiff() closure
        exec "Gdiff" l:commit
    endfunc
    silent let [gitRoot; _] = systemlist("git rev-parse --show-toplevel")
    silent let changedFiles =
        \ map(
           \ systemlist("git diff --name-only " . l:commit),
           \ { _, fn -> gitRoot ."/". fn },
        \)
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
