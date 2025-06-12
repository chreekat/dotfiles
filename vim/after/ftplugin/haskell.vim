setlocal iskeyword+='
setlocal includeexpr=(substitute(v:fname,'\\.','/','g').'.hs')

compiler! ghc

syn sync fromstart

augroup after_haskell
    autocmd! * <buffer>
    autocmd BufWritePost <buffer> call s:update_tags(expand("<afile>"))
augroup END

function! s:update_tags(fn)
    if filewritable("tags") == 1
        call job_start(
            \["/bin/sh", "-c", "fast-tags " . a:fn],
            \{"in_io":"null","out_io":"null","err_io":"null"})
    endif
endfunc

function! s:choose_compiler()
    let choices = { 'stack': 'stack build'
                 \, '&pedantic stack': 'stack build --pedantic'
                 \, 'pedantic stack &test': 'stack test --fast --pedantic'
                 \, 'hlint': 'hlint\ %'
                 \}
    let choice = confirm("Which compiler?", join(keys(choices), "\n"))
    let compiler = choices[keys(choices)[choice - 1]]
    let &mp = l:compiler
endfunction
nnoremap <buffer> <F5> :call <SID>choose_compiler()<cr>

command! -buffer Hlint cexpr system("hlint ".expand("%"))
command! -buffer -range Dollar sil <line1>|sil exe "normal $F$caw("|sil <line2>|sil normal A)
command! -buffer -range=% Hindent <line1>,<line2>!hindent --indent-size 4

" Buffer global state to keep foldlevel calculations fast.
let b:importFirst = -1
let b:importLast = -1
let b:exportFirst = -1
let b:exportLast = -1

" Update the global state often.
augroup ImportRange
    autocmd! * <buffer>
    autocmd BufReadPost,CursorHold,CursorHoldI <buffer> let [b:importFirst, b:importLast] = s:ImportRange()
    autocmd BufReadPost,CursorHold,CursorHoldI <buffer> let [b:exportFirst, b:exportLast] = s:ExportRange()
augroup END

" Find the first and last lines that start with 'import' in the file.
func s:ImportRange()
    return s:ThingRange('^import', '^import')
endfunc

func s:ExportRange()
    return s:ThingRange('^module', 'where')
endfunc

func s:ThingRange(start, end)
    let lnum = 1
    let first = -1
    let last = -1

    let eob = line('$')

    while lnum <= l:eob
        let line = getline(lnum)
        let first = lnum
        let lnum += 1
        if line =~ a:start
            break
        endif
    endwhile

    while lnum <= eob
        let line = getline(lnum)
        if line =~ a:end
            let last = lnum
        elseif line =~ '^\S'
            break
        endif
        let lnum += 1
    endwhile

    return [first, last]
endfunc

" Fold all imports in a single level-1 fold.
func s:ImportFold(lnum)
    if b:importFirst == -1
        let [b:importFirst, b:importLast] = s:ImportRange()
    endif
    if b:exportFirst == -1
        let [b:exportFirst, b:exportLast] = s:ExportRange()
    endif

    if a:lnum >= b:importFirst && a:lnum <= b:importLast
        return '1'
    elseif a:lnum >= b:exportFirst && a:lnum <= b:exportLast
        return '1'
    elseif getline(a:lnum) =~ '^$'
        return '0'
    else
        return '='
    endif
endfunc

setl foldmethod=expr
setl foldexpr=s:ImportFold(v:lnum)
setl fdc=3
setlocal formatoptions=croqnlj
normal `"
