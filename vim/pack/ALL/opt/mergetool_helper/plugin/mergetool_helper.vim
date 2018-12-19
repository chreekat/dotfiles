set swb=useopen

"
" Compare different pairs of windows. Dual mappings, so it doesn't matter what
" order you specify them in.
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

" a = all
nnoremap <leader>ca :windo difft<cr>


cnoremap <SID>VUMS <<<<<\\|>>>>>\\|=====<cr>
" ^ 'Vimrc Unimpaired Marker Search'
map ]C /<SID>VUMS
map [C ?<SID>VUMS
