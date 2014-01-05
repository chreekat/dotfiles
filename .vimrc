" Vundle nonsense
set nocompatible
filetype off

if isdirectory($HOME."/.vim/bundle/vundle")
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()

    " let Vundle manage Vundle
    " required!
    Bundle 'gmarik/vundle'
    Bundle 'tpope/vim-fugitive'
    Bundle 'tpope/vim-surround'
    Bundle 'scrooloose/nerdcommenter'
    Bundle 'tpope/vim-unimpaired'
    Bundle 'tpope/vim-speeddating'
    Bundle 'tpope/vim-repeat'

    let g:syntastic_coffee_lint_options = "-f ~/Dropbox/bDotfiles/coffeelint.json"
    let g:syntastic_always_populate_loc_list=1
    Bundle 'scrooloose/syntastic'
    Bundle 'kchmck/vim-coffee-script'
    Bundle 'altercation/vim-colors-solarized'
    Bundle 'a.vim'
    Bundle 'msanders/snipmate.vim'
    Bundle 'godlygeek/tabular'
    Bundle 'panozzaj/vim-autocorrect'
    Bundle 'tpope/vim-markdown'

    "let g:instant_markdown_slow = 1
    "Bundle 'suan/vim-instant-markdown'
    "Bundle 'lukaszkorecki/workflowish'
    Bundle 'b4winckler/vim-angry'
    "Bundle 'Lokaltog/vim-powerline'
    Bundle 'kergoth/vim-hilinks'

    let g:ctrlp_map = '<leader>t'
    Bundle 'kien/ctrlp.vim'
    "Bundle 'genutils'
        "Bundle 'vim-scripts/BreakPts'
    Bundle 'chreekat/vim-paren-crosshairs'
    Bundle 'chreekat/vim-colors-lunatic'
    Bundle 'vim-scripts/VisIncr'
    " Bundle 'pbrisbin/html-template-syntax'
    Bundle 'merijn/haskellFoldIndent'
    Bundle 'Shougo/vimproc.vim'

    Bundle 'sjl/gundo.vim'
    Bundle 'bruno-/vim-vertical-move'
    Bundle 'sjl/splice.vim'
    Bundle 'tpope/vim-abolish'
else
    echomsg "Vundle not installed! Hecka weirdness may ensue."
endif
"set rtp+=~/.local/lib/python2.7/site-packages/powerline/bindings/vim

syn enable

ru macros/matchit.vim

filetype plugin indent on

if isdirectory($HOME."/.vim/bundle/vim-colors-lunatic")
    colorscheme lunatic
else
    echomsg "Skipping colorscheme cause it's no-findings."
endif

let g:is_bash = 1
let mapleader = ","

set ai
" set cpo+=J
set cpo+=n
set dict=/usr/share/dict/words
set equalalways
set et
set fillchars+=stl:=,stlnc:-
set fo+=l
set foldtext=BFoldtext()
set joinspaces!
set gp=ack-grep\ -H\ --column
set grepformat=%f:%l:%c:%m
set hidden
set modeline
set is
" SO MUCH BETTER
set isfname-==
set laststatus=2 " Always show status
set list
set lcs=tab:>\ ,trail:⋅,extends:<,precedes:>
set mouse=
set showbreak=<<<
set showcmd
set sidescroll=1
"set sidescrolloff=3
set smartcase ignorecase
" set statusline=%f%m%r%h%w\%=[L:\%l\ C:\%c\ P:\%p%%]
set number
set nosol
set sw=2
set swb=useopen
set titlestring=vi:\ %t%(\ %M%)%(\ (%{expand(\"%:~:.:h\")})%)%(\ %a%)
set tw=75
" In testing!
set virtualedit=onemore
set wildmode=longest:list:longest,list:full
set wildignore+=*.o,*.hi,dist
"set wiw=40 nowrap " For shoots and googles
let &wiw = &tw
set whichwrap=<,>,[,] " arrow keys wrap
set exrc
if has("persistent_undo")
    set undodir=~/.vim/undos
    set undofile
endif


let g:sh_fold_enabled=1
let g:tex_flavor="latex"
let g:Tex_DefaultTargetFormat="pdf"

let g:haddock_browser = "/usr/bin/google-chrome"
let g:haddock_indexfiledir = "~/.vim"

map <leader>xo :!xdg-open %<cr><c-l>

map <F5> :GundoToggle<cr>

map <Leader>e zfaB

nmap <Leader>V :tabe  ~/LoByMyHand/bDotfiles/.vimrc<cr>
nmap <Leader>S :so ~/.vimrc<cr>
nmap <Leader>L :tabe ~/LoByMyHand/bDotfiles/.vim/bundle/vim-colors-lunatic/colors/lunatic.vim<cr>

" Make <c-s> useful!
nmap <c-s> :up<cr>
vmap <c-s> :w
imap <c-s> <esc>:up<cr>

" Escape key? No thanks
vmap <Tab> <Esc>
imap <Tab> <Esc>
omap <Tab> <Esc>
" Catches a standard fuck up I do:
nmap r<Tab> <Esc>

" Adds C-u to the undo stream:
inoremap <C-u> <esc>S

nmap <C-w>M <C-w>\|<C-w>_

" Toggle numbers. Maybe useless? Let's find out.
map <leader># :if &nu \| set rnu \| elseif &rnu \|  set rnu! \| else \| set nu \| endif<cr>

" From :help E447
map gf :e <cfile><cr>

nmap <leader>u :windo update<cr>

" Swap two words, like <m-t> in emacs
nmap \t l2bdiw"_xea<space><esc>p

" There is never a time I don't want this, I believe.
"noremap j gj
"noremap k gk
"imap <up> <c-o>k
"imap <down> <c-o>j
"nmap <up> k
"nmap <down> j
" These don't work nice with nowrap, though
"noremap 0 g0
"noremap $ g$

" Quick toggle of hls
function! ToggleHighlight()
    set hls!
    if &hls
        let @/ = input("Search: ", @/)
    endif
endfunction

nmap /// :call ToggleHighlight()<cr>

" I just can't type z
nmap ,. z

" Brackets suck*
nmap ,g [
nmap ,c ]

" Colon sucks
nmap ,p :

" * By "suck", I mean I suck at typing them correctly

" The "Dominate Dragons" idea
command! GQ silent Gcommit -am "quicksave"

" Makes the Alternate plugin (a.vim) easier for Dvorak
command! Z A

" A command to advance to the next vimdiffem'd file.
command! Go diffo|on|n|Gdiff
command! Gop diffo|on|prev|Gdiff

" unicode length, from https://github.com/gregsexton/gitv/pull/14
if exists("*strwidth")
  "introduced in Vim 7.3
  fu! StringWidth(string)
    return strwidth(a:string)
  endfu
else
  fu! StringWidth(string)
    return len(split(a:string,'\zs'))
  endfu
end

function! WindowWidth()
  let pad = 2

  " Calc existence of sign column
  redir => signs
  exec "silent sign place buffer=".bufnr('%')
  redir END
  if match(signs, '\n    line') >= 0
    let signColumn = 2
  else
    let signColumn = 0
  endif

  return winwidth(0) - &fdc - &number*&numberwidth - signColumn - pad
endfunction

function! BFoldtextRealz(foldstart, foldend)
    let lines = a:foldend - a:foldstart
    let commentPat = substitute(&cms, '%s', '.*', '')

    let firstline=getline(a:foldstart)
    " Now, a bunch of replacements/removals
    "   End-of-line Comments
    let firstline = substitute(firstline, '\S\+\zs\s*'.commentPat, '', 'g')
    "   Syntactic white noise
    let firstline = substitute(firstline, '^class\s*', '', 'g')

    let textend = '|' . lines . '| ↓' . v:foldlevel

    " Now, chop off as much of the firstline as necessary to show the fold info.
    let windowWidth = min([WindowWidth(), 80])
    let lineWidth = StringWidth(firstline)
    let endWidth = StringWidth(textend)

    if windowWidth < (lineWidth + endWidth)
        let firstline = strpart(firstline, 0, windowWidth - endWidth - 1)
        let firstline .= "…"
    endif

    return firstline . repeat(" ", windowWidth-StringWidth(firstline.textend)) . textend
endfunction
function! BFoldtext()
    return BFoldtextRealz(v:foldstart, v:foldend)
endfunction

func! s:emptyFile(fname)
    return getfsize(a:fname) == 0 && !isdirectory(a:fname)
endfunc

function! WordCount()
    let l:old_status = v:statusmsg
    let l:position = getpos(".")
    exe ":silent normal g\<c-g>"
    let l:stat = v:statusmsg
    let l:word_count = 0
    if l:stat != '--No lines in buffer--'
        let l:word_count = str2nr(split(v:statusmsg)[11])
        let v:statusmsg = l:old_status
    end
    call setpos('.', l:position)
    return l:word_count
endfunction

fu! NanoStats()
    let l:day_num = str2nr(strftime('%-d'))
    let l:goal = 50000.0

    "let l:goal_today = float2nr(ceil(50000.0*l:day_num/30) + 0.01)
    let l:daily_goal = l:goal / 30
    let l:goal_today = l:goal * l:day_num / 30

    let l:words_today = WordCount()
    let l:words_yesterday = system(
                \ 'git log --before=0:00 --pretty=oneline -n 1 '
                \ . '| perl -ne "m%(\d+)/\d+% and print \$1"')
    let l:diff_today = l:words_today - l:words_yesterday

    let l:daily_pct = 100 * l:diff_today / l:daily_goal
    let l:pct_today = 100 * l:words_today / l:goal_today
    let l:pct = 100 * l:words_today / l:goal

    return {
          \ 'diff_today': l:diff_today,
          \ 'daily_pct': l:daily_pct,
          \ 'words_today': l:words_today,
          \ 'goal_today': l:goal_today,
          \ 'pct_today': l:pct_today,
          \ 'words_yesterday': l:words_yesterday,
          \ 'pct': l:pct
          \ }
endfu

fu! NanoStatus()

    let l:stats = NanoStats()
    return printf(
                \ '+%.0f(%.2f%%) today, %.0f/%.0f(%.2f%%) to date, %.2f%% overall',
                \ l:stats.diff_today, l:stats.daily_pct,
                \ l:stats.words_today, l:stats.goal_today,
                \ l:stats.pct_today,
                \ l:stats.pct)
endfu

fu! NanoDayPct(yesterday)
    if !exists("b:nanoDayPct")
        let b:nanoDayPct = "--"
    endif

    if index(["n", "i"], mode()) < 0
        return b:nanoDayPct
    endif

    let l:pct = 100 * (WordCount() - a:yesterday) / 1667
    let l:res = printf("%d%%", l:pct - l:pct % 5)
    let b:nanoDayPct = l:res
    return l:res
endfu

fu! Nanoize()
    let l:stats = NanoStats()
    let b:words_yesterday = l:stats.words_yesterday
    setl statusline=%<%f\ [%{NanoDayPct(b:words_yesterday)}]\ %h%m%r%=%-14.(%l,%c%V%)\ %P
endfu

fu! SoftWordWrap()
    setl nolist lbr tw=0 wrap
    nmap 0 g0
    nmap $ g$
    noremap g0 0
    noremap g$ $
endfu
command! SoftWordWrap call SoftWordWrap()

augroup vimrc
    au!
    " Move to last-known position when entering file. See :he 'quote
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " New file? Immediately start insert mode.
    " NOPE.  This fucks up too many things (e.g. command-t)
    " au BufNewFile * startinsert

    " Empty file? Start insert mode
    au BufEnter * if s:emptyFile(expand("%")) | star | endif

    au BufEnter ~/src/Elm/* setl ts=8 noet noeol
    au BufEnter ~/src/serenade.js/*.coffee setl sw=2
    au BufEnter ~/src/angular-phonecat/* setl sw=2
    au BufEnter ~/src/tasty/* setl sw=2

    " Reset compiler in case the file was renamed, since compiler has the
    " filename hardcoded thanks to vim-coffee-script making nearsighted
    " accomodations for mis-named files.
    "au BufWrite ~/LoByMyHand/*.coffee compiler coffee|silent make

    " call SoftWordWrap() |
    au BufRead nanowrimo.txt nmap ,ct :echo NanoStatus()<cr>
                \|nmap ,Cc :exec 'Gcom -m "' . escape(NanoStatus(), '%') . '" %'<cr>
                \|nmap ,Ca :exec 'Gcom --amend -m "' . escape(NanoStatus(), '%') . '" %'<cr>
                \|setl tw=72 fo+=a fp=par
                \|call Nanoize()
                " \|ru autocorrect.vim | ru dvorak.vim

    au BufNewFile ~/Dropbox/Project_Euler/p*.lhs :0r <abuf>:h/problem.skel |4
    au BufNewFile *.html :0r ~/.vim/skel/html
    au BufNewFile *.hamlet :0r ~/.vim/skel/hamlet
    au BufNewFile ~/bin/* :0r ~/.vim/skel/bin | setf sh | 5
aug END

" This needs to be better... needs to reuse quickfix buffer.
"aug qf
"    au!
"    au QuickFixCmdPre * tabnew
"aug END

"hi!  clear SpecialKey
"hi! SpecialKey ctermfg=160 ctermbg=240
