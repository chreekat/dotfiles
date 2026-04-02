vim9script

# Shared hledger account completion for Log.txt and .journal files

# Exported for testing
export def FindMainJournal(): string
    # Check PWD first, then buffer's directory
    for dir in [getcwd(), expand('%:p:h')]
        var path = dir .. '/main.journal'
        if filereadable(path)
            return path
        endif
    endfor
    return ''
enddef

# Exported for testing
export def GetAccounts(): list<string>
    var main = FindMainJournal()
    if !empty(main)
        # Use hledger accounts command
        return systemlist('hledger accounts -f ' .. shellescape(main))
    else
        # Fall back to current buffer extraction
        return uniq(sort(mapnew(
            matchbufline(bufnr(), '^\S.\{-}\ze  ', 1, '$'),
            (_, val) => val["text"]
        )))
    endif
enddef

export def CompleteAccount(findstart: number, base: string): any
    if findstart == 1
        var line = getline('.')
        if strlen(line) > 0
            return match(line, '\S')
        else
            return -3
        endif
    else
        return matchfuzzy(GetAccounts(), base)
    endif
enddef
