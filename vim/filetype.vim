if did_filetype()
    finish
endif
augroup filetypedetect
    au! BufRead,BufNewFile *.\(journal\|ledger\) setfiletype journal
    au! BufRead,BufNewFile *.letter setfiletype mail
augroup END
