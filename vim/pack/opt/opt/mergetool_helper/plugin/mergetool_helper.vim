set swb=useopen

"
" Compare different pairs of windows. Dual mappings, so it doesn't matter what
" order you specify them in.
"

"
" USAGE:
"
" First, add the following to your git config:
"
"     [mergetool "mergetool_helper"]
"         cmd = "vim -f -d $LOCAL $BASE $REMOTE $MERGED -c '4wincmd w | wincmd J | packadd mergetool_helper'"
"         trustExitCode = true
"
" Note the use of :packadd.
"
" Then, either specify the tool with `git mergetool --tool mergetool_helper`, or
" set it as the default by setting merge.tool = mergetool_helper. See
" git-config(1) for details.
"
" Tip: You can abort a resolution with :cq.
"

" lb = LOCAL, BASE
nnoremap <SID>CLB :windo diffo<cr>:sb BASE<cr>:difft<cr>:sb LOCAL<cr>:difft<cr>
nmap <leader>clb <SID>CLB
nmap <leader>cbl <SID>CLB

" rb = REMOTE, BASE
nnoremap <SID>CRB :windo diffo<cr>:sb BASE<cr>:difft<cr>:sb REMOTE<cr>:difft<cr>
nmap <leader>crb <SID>CRB
nmap <leader>cbr <SID>CRB

" lr = LOCAL, REMOTE
nnoremap <SID>CLR :windo diffo<cr>:sb LOCAL<cr>:difft<cr>:sb REMOTE<cr>:difft<cr>
nmap <leader>clr <SID>CLR
nmap <leader>crl <SID>CLR

" lm = LOCAL, MERGED
nnoremap <SID>CLM :windo diffo<cr>:sb LOCAL<cr>:difft<cr>:4wincmd w<cr>:difft<cr>
nmap <leader>clm <SID>CLM
nmap <leader>cml <SID>CLM

" rm = REMOTE, MERGED
nnoremap <SID>CRM :windo diffo<cr>:sb REMOTE<cr>:difft<cr>:4wincmd w<cr>:difft<cr>
nmap <leader>crm <SID>CRM
nmap <leader>cmr <SID>CRM

" bm = BASE, MERGED
nnoremap <SID>CBM :windo diffo<cr>:sb BASE<cr>:difft<cr>:4wincmd w<cr>:difft<cr>
nmap <leader>cbm <SID>CBM
nmap <leader>cmb <SID>CBM

" a = all
nnoremap <leader>ca :windo difft<cr>

" n = none
nnoremap <leader>cn :windo diffoff<cr>

" TODO: Use search() to avoid overwriting "/.
cnoremap <SID>VUMS <<<<<\\|>>>>>\\|=====<cr>
" ^ 'Vimrc Unimpaired Marker Search'
map ]C /<SID>VUMS
map [C ?<SID>VUMS
