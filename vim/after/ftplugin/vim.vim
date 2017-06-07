" Match v:errors strings
" e.g.:
" /home/b/src/Haskell/intero/vim/autoload/intero.vim line 47: bleb: Expected True but got 0
if exists("b:did_after_ftplugin")
    finish
endif
let b:did_after_ftplugin = 1

set efm^=%f\ line\ %l:\ %m
