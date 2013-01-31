let b:autoviewFile = expand('%:p:h') . '/autoview.' . expand('%')
let b:detritus = expand ('%:p:h') . '/autoview.' . expand('%:r')

fu! s:update()
    silent exec("w! " .b:autoviewFile)
endfu

fu! s:cleanup()
    call system("rm -f " . b:detritus . ".*")
endfu

aug TexAutoview
    au!
    au InsertLeave,BufWrite <buffer> call s:update()
    au BufUnload <buffer> call s:cleanup()
aug END

call s:update()
call system("latexmk -pvc " . shellescape(b:autoviewFile) . " >/dev/null 2>&1 &")
