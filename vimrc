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
runtime shifted_fkeys.vim

"" My preferences
filetype indent off
filetype plugin on
set completeopt+=menuone
set foldopen=
" * Allow single letters as items, too. (This will break if a sentence ends with
"   "... I.", and ends on a newline, but c'est la vie.
" * Require some kind of list marker, not just whitespace
" * Require whitespace after the marker
let &formatlistpat = '^\s*\(\d\+\|[[:alnum:]]\)[\]:.)}]\s\+'
set history=5000
set incsearch
set laststatus=1
set nojoinspaces
set pastetoggle=<F2>
set path=.,,
" Try to make sure :hardcopy doesn't work, because it sux
set printdevice=do_not_use
set showtabline=1
set showfulltag
set splitbelow
" Ignored Haskell suffixes
set suffixes+=.dyn_hi-boot,hi-boot,o-boot,hs-boot
" Prefer things with suffixes
set suffixes+=,
set updatetime=100

"" My anti-preferences
" I used to think 'useopen' was a decent default, but it messes up :stag and
" friends when jumping to a tag in the same buffer. That's weird and
" unfortunate.
"set switchbuf=useopen

packadd! matchit

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
"" Optimizations
""

" This is generally fast enough, and less annoying
syn sync fromstart


""
"" S H O R T C U T S
""

let mapleader = " "

" (Note pastetoggle=<F2>)

" Format all the things
nnoremap <leader><cr> gw
nnoremap <leader><cr><cr> gwap

" More useful (?) F1
function! s:toggle_colors()
    let i = index(g:colors_list, g:colors_name)
    let n = len(g:colors_list)
    if i >= 0
        exec "colorscheme " . g:colors_list[(i + 1) % n]
    endif
endfunction

noremap <F1> :call <SID>toggle_colors()<cr>
imap <F1> <c-o><F1>

" Jump to vimrc
nnoremap <F3> :tabe ~/.vimrc<cr>

" Open the undotree
nnoremap <F4> :UndotreeToggle<cr>

" Open url (or anything, I guess) at point. Interwebs, you say?
nnoremap <F6> :call system(printf("xdg-open %s &", expand('<cWORD>')))<cr>

" Insert today's date, in two formats
inoremap <expr> <F9> strftime("%Y-%m-%d")
inoremap <expr> <S-F9> strftime("%Y%m%d")

" Insert the time
inoremap <expr> <F10> strftime("%H:%M")
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
    let l:winline = winline()
    setl nofoldenable
    exec 'normal "' . a:reg . ']' . a:dir
    let c = a:count
    while c > 0
        exec 'normal ' . a:dent . "']"
        let c = c - 1
    endwhile
    set foldenable
    " Reset view
    exec "normal zt" . l:winline . "\<c-y>"
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

" Trim whitespace thx (and delete blank lines at EOF)
command! -range Trim <line1>,<line2>s/\s\+$\|\n\+\%$//e
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
    au BufRead IN.in runtime IN.in.vim
    " Show context in quickfix for GHC's sake
    au BufReadPost quickfix setl so=3

    " Why isn't this here?
    au BufRead,BufNewFile Vagrantfile setf ruby

    "" SKELETON FILES
    au BufNewFile *.sh 0r ~/.vim/skel/skel.sh|normal G
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
augroup vimrc_highlighting
    au!
    au ColorScheme * hi Comment ctermfg=137
    au ColorScheme default hi Pmenu ctermfg=NONE
    " ^ Term colorscheme makes this invisible otherwise
    au ColorScheme default hi Folded NONE
    au ColorScheme default hi Folded ctermfg=247
    au ColorScheme default hi DiffAdd NONE
    au ColorScheme default hi DiffDelete NONE
    au ColorScheme default hi DiffText NONE
    au ColorScheme default hi DiffChange NONE
    au ColorScheme default hi DiffAdd ctermfg=28 cterm=reverse
    au ColorScheme default hi DiffDelete ctermfg=203 cterm=reverse
    au ColorScheme default hi DiffText ctermfg=173 cterm=reverse
    au ColorScheme default hi DiffChange ctermfg=104 cterm=reverse
augroup END
doautocmd ColorScheme
colorscheme apprentice
" Used in toggle_colors below
let g:colors_list = ["apprentice", "default"]

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

" M: Ranged search (M like perl's m///)
command! -range=% -nargs=1 M normal /\%><line1>l\%<<line2>l<args><cr>

" Columns: romainl's quick tabular alignment
command! -range Columns :<line1>,<line2>!column -t -o ' '

" Hardcopy: Use iconv and enscript to dump thy text
command! -range=% Hardcopy :<line1>,<line2>w !iconv -f UTF-8 -t 8859_1//TRANSLIT | enscript -2rBh

" Simplistic mappings for making headers
noremap <leader>h1- yy2pkv$r-jjv$r-
noremap <leader>h1# yy2pkv$r#jjv$r#
noremap <leader>h1" yy2pkv$r"jjv$r"
noremap <leader>h1= yy2pkv$r=jjv$r=

noremap <leader>h2 yypv$r
