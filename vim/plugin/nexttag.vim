command! NextTag call s:nexttag()

function! s:nexttag()
    " Return here on error.
    let l:startpos = getpos(".")

    " Start at the next word
    normal w

    let l:searching = 1
    while l:searching
        " Look for tags for the current word
        try
            exe "silent tselect " . expand("<cword>")
            let l:searching = 0
        " Continue to next word if no tags exist
        catch /E426/
            let l:pos = getpos(".")
            normal w
            if l:pos == getpos(".")
                call setpos('.', l:startpos)
                let l:searching = 0
                echo "No more tags"
            endif
        endtry
    endwhile
endfunction
