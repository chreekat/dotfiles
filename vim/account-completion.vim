vim9script

# if exists("b:did_ftplugin")
#   finish
# endif
# b:did_ftplugin = 1
# b:undo_ftplugin = ""

def CompleteTimeAccount(findstart: number, base: string): any
    if findstart == 1
        if IsAcct(getline('.'))
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

#ci:pipeline config:cabal  2
setlocal omnifunc=CompleteTimeAccount
