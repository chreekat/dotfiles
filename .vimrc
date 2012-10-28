" Vundle nonsense
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-speeddating'
Bundle 'matchit.zip'
Bundle 'kchmck/vim-coffee-script'
Bundle 'altercation/vim-colors-solarized'
Bundle 'a.vim'

filetype plugin indent on
"syntax enable

let xml_jump_string = "&$&"
"call pathogen#infect()
set t_Co=256
let g:solarized_bold = 0
colorscheme solarized
set aw
set ai
set bg=dark
set et
set fdc=3
set gp=ack-grep
set hidden
set is
set laststatus=2 " Always show status
set list
set lcs=tab:>\ ,trail:⎵
set mouse=
set showcmd
set statusline=%f%m%r%h%w\%=[L:\%l\ C:\%c\ A:\%b\ H:\x%B\ P:\%p%%]
set sol!
set sw=4
set swb=usetab
set titlestring=vi:\ %t%(\ %M%)%(\ (%{expand(\"%:~:.:h\")})%)%(\ %a%)
set ts=4
set tw=80
set wildmode=longest,list

let mapleader = ","
let g:sh_fold_enabled=1
let g:tex_flavor="latex"
let g:Tex_DefaultTargetFormat="pdf"

let g:haddock_browser = "/usr/bin/google-chrome"
let g:haddock_indexfiledir = "~/.vim"

map <Leader>e zfaB
map <Leader>S :so ~/.vimrc<cr>
map! <C-s> <Esc>:up<cr>
nmap <C-s> :up<cr>
vmap <Tab> <Esc>
imap <Tab> <Esc>
omap <Tab> <Esc>
" Catches a standard fuck up I do:
nmap r<Tab> <Esc>
" Adds C-u to the undo stream:
imap <C-u> <C-o>S
nmap <C-w>M <C-w>\|<C-w>_

" Makes the Alternate plugin (a.vim) easier for Dvorak
command! Z A

" A command to advance to the next vimdiffem'd file.
command! Go diffo|on|n|Gdiff
command! Gop diffo|on|prev|Gdiff

func! s:emptyFile(fname)
    return getfsize(a:fname) == 0 && !isdirectory(a:fname)
endfunc


augroup vimrc
    au!
    " Move to last-known position when entering file. See :he 'quote
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " New file? Immediately start insert mode.
    au BufNewFile * startinsert

    " Empty file? Start insert mode
    au BufEnter * if s:emptyFile(expand("%")) | star | endif
aug END

" This needs to be better... needs to reuse quickfix buffer.
"aug qf
"    au!
"    au QuickFixCmdPre * tabnew
"aug END

"hi!  clear SpecialKey
"hi! SpecialKey ctermfg=160 ctermbg=240
