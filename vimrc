" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.vim/bundle')
    Plug 'tpope/vim-flagship'
    Plug 'tpope/vim-fugitive'
call plug#end()

" Sane defaults
set expandtab
set ttimeoutlen=20

" My preferences
set incsearch
set foldopen-=search

" In the absence of file- or filetype-specific options, these are the defaults
" I want.
set shiftwidth=4
set autoindent
set textwidth=80

""
"" Scheme Usability Tweaks
""
"" These don't change the scheme so much as the UI.
hi clear Folded
hi Folded ctermfg=16777200
