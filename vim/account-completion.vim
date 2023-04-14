vim9script

# if exists("b:did_ftplugin")
#   finish
# endif
# b:did_ftplugin = 1
# b:undo_ftplugin = ""

def CompleteTimeAccount(findstart: number, base: string): any
    if findstart == 1
        if IsAcct()
            return 0
        else
            # Cancel silently and leave completion
            return -3
        endif
    else
        return GetAccts(base)
    endif
enddef

def IsAcct(): bool
    var line = getline('.')
    var leading = line[0 : col('.') - 1]
    return leading == '' || leading =~ '^[a-z]'
enddef

def GetAccts(base: string): list<string>
    var matches = []
    var lineNr = 0
    # The end of a account is the first two spaces
    var endmatch = '.\{-}\ze  '
    var match = '^\S*' .. base .. endmatch
    if base == ""
        match = '^[a-z]' .. endmatch
    endif
    while lineNr < line('$')
        var account = matchstr(getline(lineNr + 1), match)
        if account != ""
            add(matches, account)
        endif
        lineNr += 1
    endwhile
    return sort(matches)
enddef

setlocal omnifunc=CompleteTimeAccount
