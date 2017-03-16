call plug#begin('~/.vim/bundle')
    Plug 'chrisbra/NrrwRgn', { 'on': 'NR' }
    Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
    Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
    Plug 'tpope/vim-flagship'
    Plug 'tpope/vim-fugitive'
    Plug '~/LoByMyHand/vim-simple-md'
    Plug '~/LoByMyHand/passhole'
    Plug '~/LoByMyHand/vimin'
call plug#end()

""
"" C O N F I G U R A T I O N
""

"" Sane defaults
set expandtab
set formatoptions+=j
set hidden
set modeline
" ^ Debian apparently resets modeline, which is on by vim default
set showcmd
set ttimeoutlen=20
if has("persistent_undo")
    set undodir=~/.vim/undos
    set undofile
endif
set wildignore+=*.o,*.hi,dist,*.dyn_o,*.dyn_hi,.git,.stack-work
set wildmode=list,longest,full

"" My preferences
set foldopen-=search
set incsearch
set nojoinspaces
set pastetoggle=<F2>
set path=.,,**
" Make s a synonym for z, which I always mistype
nnoremap s z
nnoremap ss zz

"" My plugin preferences
let g:undotree_WindowLayout = 4

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
"" S H O R T C U T S
""

" Open the undotree
nnoremap <F5> :UndotreeToggle<cr>

" Insert today's date.
inoremap <F9> <c-r>=system("date +%Y-%m-%d $@ \| perl -pe chomp")<cr>
" Insert the time
inoremap <F10> <c-r>=system("date +%H:%M $@ \| perl -pe chomp")<cr>
" Both! :D
imap <F11> <F9>T<F10>

" Unimpaired-inspired maps
nnoremap ]q :cnext<cr>
nnoremap [q :cprev<cr>
nnoremap ]j :lnext<cr>
nnoremap [j :lprev<cr>

" Trim whitespace thx
command! -range Trim <line1>,<line2>s/\s\+$//
command! TRIM %Trim


""
"" O T H E R   C O N F I G U R A T I O N
"" 

" I often interchange keys while typing in Dvorak (well, maybe I'd always do it,
" but..
runtime dvorak-fuckups.vim

"" FILE SPECIFIC SETTINGS
augroup vimrc
    au!
    au BufRead ~/.hledger.journal runtime hledger-main-journal.vim
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
    hi clear DiffText
    hi clear Folded
    hi Folded ctermfg=16777200
    hi Search ctermbg=404055
endfunction
augroup vimrc_highlighting
    au!
    au ColorScheme * call s:vimrc_highlighting() 
augroup END
doautocmd ColorScheme
