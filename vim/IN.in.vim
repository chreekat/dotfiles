aug ING
au!
au BufEnter <buffer> call system("tmux renamew IN")
au BufUnload <buffer> call system("tmux setw automatic-rename")
aug END

" Refresh @w, which "imports" a workflowy export
let @w = "2] \"+pdapvip:s/-/——/\<cr>gv>kdd"
