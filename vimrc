""
"" C O N F I G U R A T I O N
""

"" Sane defaults
set backspace=indent,eol,start
set expandtab
set formatoptions+=j
set hidden
set modeline
" ^ Debian apparently resets modeline, which is on by vim default
set shiftround
" ^ Make << and >> act like i_^t and i_^d
set showcmd
set switchbuf=useopen
set ttimeoutlen=20
if has("persistent_undo")
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
set splitright

packadd! matchit

" | Make s a synonym for z, which I always mistype
nmap s z
nmap ss zz
vmap s z

"" My plugin preferences
let g:goyo_width = 84
let g:undotree_WindowLayout = 4
let g:easy_align_delimiters = {'>': {'pattern': '::\|->\|=>'}}

"" In the absence of file- or filetype-specific options, these are the defaults
"" I want.
let g:is_bash=1
set autoindent
set shiftwidth=4
set textwidth=80
set formatoptions+=nl

""
"" Tool integrations
""
set grepprg=rg\ -H\ --vimgrep
set grepformat=%f:%l:%c:%m,%f:%l:%m

""
"" Behavior tweaks
""

" Make zM take a count, like G, setting an absolute foldlevel.
nnoremap zM :<c-u>let &foldlevel=v:count<cr>

" Make [[ and ]] support { being somewhere other than column 1
nnoremap [[ :call search('^\S\&.*{$', 'bsW')<cr>
nnoremap ]] :call search('^\S\&.*{$', 'sW')<cr>

""
"" S H O R T C U T S
""

" (Note pastetoggle=<F2>)

" More useful (?) F1
nnoremap <F1> :b <c-d>

" Jump to vimrc
nnoremap <F3> :tabe ~/.vimrc<cr>

" Open the undotree
nnoremap <F4> :UndotreeToggle<cr>

" Open url (or anything, I guess) at point. Interwebs, you say?
nnoremap <F6> :call system("xdg-open " . expand('<cWORD>'))<cr>

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
nnoremap ]j :lnext<cr>
nnoremap [j :lprev<cr>
cnoremap <SID>VUMS <<<<<\\|>>>>>\\|=====<cr>
" ^ 'Vimrc Unimpaired Marker Search'
" TODO: don't clobber the real ]c, [c
"map ]c /<SID>VUMS
"map [c ?<SID>VUMS

nmap ]<space> <f2>o<esc><f2>'[
nmap [<space> <f2>O<esc><f2>
" ^ Uses pastetoggle

function! VimrcIndentPaste(reg, dent, dir)
    "mkview!
    exec 'normal "' . a:reg . ']' . a:dir . a:dent . "']"
    "loadview
endfu

for dent in ['>','<']
    for dire in ['p','P']
        exec 'nnoremap ' . dent . dire . " :call VimrcIndentPaste(v:register, '".dent."', '".dire."')<cr>"
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
augroup END
"doautocmd ColorScheme
colorscheme apprentice

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

xnoremap <silent> <leader>= :<c-u>silent call Align()<cr>
