if did_filetype()
    finish
endif
augroup filetypedetect
    au! BufRead,BufNewFile *.letter setfiletype mail
    au! BufRead,BufNewFile *.hsc setfiletype haskell
augroup END
