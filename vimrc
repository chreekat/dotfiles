""
"" C O N F I G U R A T I O N
""

"" Sane defaults
set backspace=indent,eol,start
set breakat-=.,
" ^ These break numbers in half.
set expandtab
set formatoptions+=j
set hidden
set linebreak
" ^ Break lines in sensible places, not in the middle of a word
set makeprg=make\ -k
" ^ Keep going, and report all failures
set modeline
" ^ Debian apparently resets modeline, which is on by vim default
set shiftround
" ^ Make << and >> act like i_^t and i_^d
set showcmd
set ttimeoutlen=20
if has("persistent_undo")
    call mkdir($HOME . "/.vim/undos", "p", 0700)
    set undodir=~/.vim/undos
    set undofile
endif
set wildignore+=*.o,*.hi,dist,*.dyn_o,*.dyn_hi,.git,.stack-work
set wildmode=list:longest,full
" Make the whitespace after numbered list required
set formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s
runtime shifted_fkeys.vim

"" My preferences
filetype indent off
filetype plugin on
set completeopt+=menuone
set foldopen=
set history=5000
set incsearch
set laststatus=1
set nojoinspaces
set pastetoggle=<F2>
set path=.,,
set showtabline=1
set showfulltag
set splitbelow
" Ignored Haskell suffixes
set suffixes+=.dyn_hi-boot,hi-boot,o-boot,hs-boot
" Prefer things with suffixes
set suffixes+=,,

"" My anti-preferences
" I used to think 'useopen' was a decent default, but it messes up :stag and
" friends when jumping to a tag in the same buffer. That's weird and
" unfortunate.
"set switchbuf=useopen

packadd! matchit

" | Make s a synonym for z, which I always mistype
nmap s z
nmap ss zz
vmap s z

"" My plugin preferences
let g:goyo_width = 84
let g:undotree_WindowLayout = 4
let g:easy_align_delimiters = {'>': {'pattern': '::\|->\|=>'}}
let g:ledger_bin = 'hledger'
" Fold markdown by headers, etc
let g:markdown_folding = 1

"" In the absence of file- or filetype-specific options, these are the defaults
"" I want.
let g:is_bash=1
let g:sql_type_default = 'pgsql'
set autoindent
set formatoptions+=nl
set shiftwidth=4
set softtabstop=4
set textwidth=80

""
"" Tool integrations
""
set grepprg=rg\ -H\ --vimgrep
set grepformat=%f:%l:%c:%m,%f:%l:%m

""
"" Behavior tweaks
""

" Make zM take a count, like G, setting an absolute foldlevel.
" NB: <c-u> is for v:count
nnoremap zM :<c-u>let &foldlevel=v:count<cr>

" Make [[ and ]] support { being somewhere other than column 1
nnoremap [[ :call search('^\S\&.*{$', 'bsW')<cr>
nnoremap ]] :call search('^\S\&.*{$', 'sW')<cr>

""
"" S H O R T C U T S
""

let mapleader = " "

" (Note pastetoggle=<F2>)

" Format all the things
nnoremap <leader><cr> gw
nnoremap <leader><cr><cr> gwap

" More useful (?) F1
nnoremap <F1> :b <c-d>

" Jump to vimrc
nnoremap <F3> :tabe ~/.vimrc<cr>

" Open the undotree
nnoremap <F4> :UndotreeToggle<cr>

" Open url (or anything, I guess) at point. Interwebs, you say?
nnoremap <F6> :call system(printf("xdg-open %s &", expand('<cWORD>')))<cr>

" Insert today's date, in two formats
inoremap <F9> <c-r>=system("date +%Y-%m-%d $@ \| perl -pe chomp")<cr>
inoremap <S-F9> <c-r>=system("date +%Y%m%d $@ \| perl -pe chomp")<cr>

" Insert the time
inoremap <F10> <c-r>=system("date +%H:%M $@ \| perl -pe chomp")<cr>
" Both! :D
imap <S-F10> <F9>T<F10>

" Unimpaired-inspired maps
nnoremap ]q :cnext<cr>
nnoremap [q :cprev<cr>
nnoremap ]Q :cnfile<cr>
nnoremap [Q :cpfile<cr>
nnoremap ]l :lnext<cr>
nnoremap [l :lprev<cr>
nmap ]<space> <f2>o<esc><f2>'[
nmap [<space> <f2>O<esc><f2>
" ^ Uses pastetoggle

" Dr Chip's highlight group display
nmap <silent> <leader>hg   :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<cr>

" See error context
command! CC cc|cl!+3

" Yank the WORD here
nnoremap \w "+yiW

" Yank the whole damn doc
command! -range=% Y <line1>,<line2>y+

function! VimrcIndentPaste(count, reg, dent, dir)
    setl nofoldenable
    exec 'normal "' . a:reg . ']' . a:dir
    let c = a:count
    while c > 0
        exec 'normal ' . a:dent . "']"
        let c = c - 1
    endwhile
    set foldenable
endfu

for dent in ['>','<']
    for dire in ['p','P']
        " Need the <c-u> to clear the '<,'>, which we don't actually want (see
        " :help v:count)
        exec 'nnoremap ' . dent . dire . " :<c-u>call VimrcIndentPaste(v:count1, v:register, '".dent."', '".dire."')<cr>"
    endfor
endfor
unlet dire dent
" ^ Indented pasting

" Paste-and-format, returning to the top (as p and P do normally)
nmap >q >p']gq'[
nmap >Q >P']gq'[

" Trim whitespace thx
command! -range Trim <line1>,<line2>s/\s\+$//
command! TRIM %Trim

" Grep *
nmap <leader>* :grep -w <c-r><c-w><cr>

""
"" O T H E R   C O N F I G U R A T I O N
""

"" FILE SPECIFIC SETTINGS
augroup vimrc
    au!
    au BufRead ~/.hledger.journal runtime hledger-main-journal.vim
    au BufRead ~/IN.in runtime IN.in.vim
    " Show context in quickfix for GHC's sake
    au BufReadPost quickfix setl so=3
augroup END

"" GLOBAL AUTOCMDS
augroup vimrc_global
    au!
    " Create a global 'last insert' mark
    au InsertLeave * normal mZ
augroup END

""
"" Scheme Usability Tweaks
""
"" These don't change the scheme so much as the UI.
function! s:vimrc_highlighting()
    hi DiffText ctermfg=Green ctermbg=5
    hi clear Folded
    hi DiffDelete ctermfg=Red
    hi Folded ctermfg=16777200
    hi Search ctermbg=404055
    hi MatchParen ctermfg=14 ctermbg=0
endfunction
augroup vimrc_highlighting
    au!
    "au ColorScheme * call s:vimrc_highlighting()
    au ColorScheme * hi Comment ctermfg=137
augroup END
doautocmd ColorScheme
colorscheme apprentice
"set bg=light

""
"" Things that should be plugins?
""

" Function for turning space-indenting into tab-indenting
" FIXME: replace with calls to [un]expand(1)
function! SpaceToTab(numSpaces) range
    let scmd = a:firstline . "," . a:lastline . "s/^\\(\\%(@@\\)*\\)@@/\\1\\t"
    let spaces = ""
    let i = a:numSpaces
    while i > 0
        let spaces .= " "
        let i = i - 1
    endwhile
    let scmd = substitute(scmd, "@@", spaces, "g")
    try
        while 1
            sil exec scmd
        endwhile
    catch /^Vim\%((\a\+)\)\=:E486/
    endtry
endfunction

" Flatten for export
noremap -f :g/./,/^$/-join<cr>
ounmap -f

" Prompt for missing files with 'gf'
"
" Note gf takes a long-ass time if path includes ** and you run it somewhere
" with a deep directory tree. What's the best solution there? Restrict path
" before running gf? Use a timeout?
" TODO: This doesn't use includeexpr
function! GFPrompt()
    try
        normal! gf
    catch /^Vim\%((\a\+)\)\=:E447/
        if confirm("Not found. Create " . expand("<cfile>:p") . "?", "&no\n&yes") == 2
            let l:dir = expand("<cfile>:p:h")
            if !isdirectory(l:dir)
                call mkdir(l:dir, "p")
            endif
            edit <cfile>
        endif
    endtry
endfunction
nnoremap gf :call GFPrompt()<cr>

" Ranged search (M like perl's m///)
command! -range=% -nargs=1 M normal /\%><line1>l\%<<line2>l<args><cr>

" romainl's quick tabular alignment
function! Align()
	'<,'>!column -t -o ' '
	normal gv=
endfunction

xnoremap <silent> <leader>a !column -t -o ' '<cr>
