fun! scratch#copyForWeb()
    let l:tw = &tw
    setl tw=9999
    normal gggqG
    1,$yank +
    let &tw = l:tw
    silent undo
endfu
