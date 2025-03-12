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
    if findstart == 1
        if strlen(getline('.')) > 0
            return 0
        else
            # Cancel silently and leave completion
            return -3
        endif
    else
        return GetAccts(base)
    endif
enddef

def IsAcct(line: string): bool
    var leading = line[0 : col('.') - 1]
    return leading == '' || leading =~ '^[a-z]'
enddef

def GetAccts(base: string): list<string>
    var matches = []
    var lineNr = 0
    # The end of a account is the first two spaces
    var endmatch = '.\{-}\ze  '
    var match = '^'
    if strlen(base) > 0
        match = match .. '.*' .. base .. endmatch
    else
        match = match .. endmatch
    endif
    while lineNr < line('$')
        var line = getline(lineNr + 1)
        var account = matchstr(line, match)
        if IsAcct(line) && account != ""
            add(matches, account)
        endif
        lineNr += 1
    endwhile
    return sort(matches)
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

command -buffer FoldLevel echo FoldLevel(line('.'))
command -buffer Timestamp call Timestamp()
nnoremap <buffer> <F9> :Timestamp<cr>
nnoremap <buffer> <F10> $a ; s:unbilled<esc>0i
imap <buffer> <F10> <esc><F10>
setl sw=7 tw=90 fo-=a2 nowrap omnifunc=CompleteTimeAccount
setl foldexpr=FoldLevel(v:lnum) foldmethod=expr fdc=3 fdl=2

b:undo_ftplugin = getbufvar(bufnr(), 'undo_ftplugin') .. '|setl sw< tw< fo< wrap< omnifunc< fde< fdm< fdc< fdl<'
