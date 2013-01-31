hi link hsLiterateDelimiter NONE
hi hsLiterateDelimiter ctermfg=1

" Add a dot so ghc doesn't go all QQ
syn region markdownH1 matchgroup=markdownHeadingDelimiter start=".##\@!"      end="#*\s*$" keepend oneline contains=@markdownInline contained
syn region markdownH2 matchgroup=markdownHeadingDelimiter start=".###\@!"     end="#*\s*$" keepend oneline contains=@markdownInline contained
syn region markdownH3 matchgroup=markdownHeadingDelimiter start=".####\@!"    end="#*\s*$" keepend oneline contains=@markdownInline contained
syn region markdownH4 matchgroup=markdownHeadingDelimiter start=".#####\@!"   end="#*\s*$" keepend oneline contains=@markdownInline contained
syn region markdownH5 matchgroup=markdownHeadingDelimiter start=".######\@!"  end="#*\s*$" keepend oneline contains=@markdownInline contained
syn region markdownH6 matchgroup=markdownHeadingDelimiter start=".#######\@!" end="#*\s*$" keepend oneline contains=@markdownInline contained
