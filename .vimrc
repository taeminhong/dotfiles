" vim-plug section
" See https://github.com/junegunn/vim-plug/
call plug#begin()
" Make sure you use single quotes

" Defaults everyone can agree on
Plug 'tpope/vim-sensible'

" Any valid git URL is allowed
Plug 'https://github.com/junegunn/vim-github-dashboard.git'

if has('python3')
   Plug 'SirVer/ultisnips'
endif

Plug 'honza/vim-snippets'

call plug#end()
