syntax on
filetype plugin indent on
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
set backspace=2
colorscheme murphy

packloadall
silent! helptags ALL
set foldmethod=indent
set wildmenu
set wildmode=list:longest,full
let NERDTreeShowBookmarks = 1
autocmd VimEnter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") &&
            \ b:NERDTree.isTabTree()) |q | endif  
nnoremap <C-b> :CtrlPBuffer<cr>
