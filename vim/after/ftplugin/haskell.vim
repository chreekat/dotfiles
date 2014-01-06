nmap <buffer> <leader>hc :GhcModCheck<cr>:cc<cr>
nmap <buffer> <leader>hh :!haddock -o /tmp/haddock -h %<cr><c-l>
nmap <buffer> <leader>hl :call append(0, "{-# LANGUAGE  #-}")<cr>:normal gg013l<cr>i
nmap <buffer> <leader>hH :call <SID>openHaddock()<cr>

func! <SID>openHaddock()
    let module = split(getline(search("^module", "nw")))[1]
    let fn = "/tmp/haddock/" . tr(module, ".", "-") . ".html"
    if filereadable(fn)
        silent exec "!xdg-open " . fn
        redraw!
    endif
endfunc
