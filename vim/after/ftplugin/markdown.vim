" if exists('b:did_simplemd')
"     finish
" endif
" let b:did_simplemd = 1

let s:did_open_html = 0
let s:file = "/tmp/vim-simple-md.html"

func! s:convert()
    call system("pandoc -s -r markdown_github -w html -o " . s:file . ' ' . expand('%'))
endfunc

func! s:toggle()
    if !exists('b:markdownHtml')
        let b:markdownHtml = 1
        augroup simplemd
        au BufWritePost <buffer> call s:convert()
        aug END
        call s:convert()
        call system('xdg-open ' . s:file)
    else
        unlet b:markdownHtml
        augroup simplemd
        au! * <buffer>
        aug END
    endif
endfunc

command! -buffer MarkdownHtml call s:toggle()

" Override the html override. :/
setl sw=4
