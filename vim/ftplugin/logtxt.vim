vim9script

def Timestamp()
    # Clear blank lines at end of file
    :%s/\n\+\%$//e
    silent system('cd ~/.vim; cabal build log-timestamp')
    const timestamp = systemlist('cd ~/.vim; cabal exec -v0 log-timestamp', join(getline(1, '$'), "\n"))
    call append(line('$'), timestamp)
    normal G
    startinsert!
enddef

def CompleteTimeAccount(findstart: number, base: string): any
    # findstart is 1 when vim is asking where the match starts
    if findstart == 1
        if strlen(getline('.')) > 0
            return 0
        else
            # Cancel silently and leave completion
            return -3
        endif
    # otherwise it actually wants the completions
    else
        return matchfuzzy(uniq(sort(mapnew(matchbufline(bufnr(), '^\S.\{-}\ze  ', 1, '$'), (_, val) => val["text"]))), base)
    endif
enddef

def FoldLevel(lnum: number): string
    var line = getline(lnum)
    if line =~ '^# Week'
        return ">1"
    elseif line =~ '^\d\d\d\d-\d\d-\d\d'
        return ">2"
    elseif line =~ '^\d\d:\d\d:'
        return ">3"
    else
        return "="
    endif
enddef

# If the current line has the string "billed", do nothing. Otherwise, append ";
# s:unbilled" to the line and enter insert mode in front of the ";".
def AddUnbilled()
    if getline('.') =~ 'billed'
        return
    endif
    normal $a ; s:unbilled
    normal F;
    startinsert
enddef

# Vim9 functions are script-level by default. Mappings execute outside the
# script context. But commands defined inside the script context are apparently
# munged to point inside the script context. So, great?
command -buffer FoldLevel echo FoldLevel(line('.'))
command -buffer Timestamp call Timestamp()
command -buffer AddUnbilled call AddUnbilled()

nnoremap <buffer> <F9> :Timestamp<cr>
nnoremap <buffer> <F10> :AddUnbilled<cr>
imap <buffer> <F10> <esc><F10>
setl sw=7 tw=90 fo-=a2 nowrap omnifunc=CompleteTimeAccount
setl foldexpr=FoldLevel(v:lnum) foldmethod=expr fdc=3 fdl=2

b:undo_ftplugin = getbufvar(bufnr(), 'undo_ftplugin') .. '|setl sw< tw< fo< wrap< omnifunc< fde< fdm< fdc< fdl<'
