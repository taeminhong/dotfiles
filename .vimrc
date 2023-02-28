" vim-plug section
" See https://github.com/junegunn/vim-plug/
call plug#begin()
" Make sure you use single quotes

" Defaults everyone can agree on
Plug 'tpope/vim-sensible'

" Any valid git URL is allowed
Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" Multiple Plug commands can be written in a single line using | separators
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

call plug#end()
