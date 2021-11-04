syntax on
filetype plugin indent on
set guifont=Source_Code_Pro_Medium_Italic:h10
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
set backspace=2
colorscheme murphy
let mapleader = ','
set encoding=utf-8
set showcmd
set cursorline
set ruler
if &co > 80
    set number
endif
set linebreak
set display+=lastline
set laststatus=2
set autochdir

call plug#begin()

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch' : 'next',
    \ 'tag' : '0.1.155',
    \ 'do': 'bash install.sh',
    \ }
Plug 'ionide/Ionide-vim', {
    \ 'do': 'make fsautocomplete',
    \}
Plug 'fatih/vim-go'
Plug 'davidhalter/jedi-vim'
Plug 'hynek/vim-python-pep8-indent'
Plug 'sillybun/vim-repl'"
Plug 'Townk/vim-autoclose'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-vinegar'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mileszs/ack.vim'
Plug 'easymotion/vim-easymotion'
Plug 'Shougo/vimproc', {'do' : 'make'}
Plug 'eagletmt/ghcmod-Vim'
let g:plug_timeout = 300
Plug 'ycm-core/YouCompleteMe', { 'do': './install.py' }
Plug 'mbbill/undotree'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
Plug 'EinfachToll/DidYouMean'
Plug 'ervandew/supertab'
Plug 'justmao945/vim-clang'
Plug 'vim-scripts/SingleCompile'

call plug#end()

packloadall
silent! helptags ALL
set foldmethod=indent
set wildmenu
set wildmode=list:longest,full
"let NERDTreeShowBookmarks = 1
"autocmd VimEnter * NERDTree
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") &&
"            \ b:NERDTree.isTabTree()) |q | endif  
nnoremap <C-b> :CtrlPBuffer<cr>

let g:PyFlakeOnWrite = 1
let g:PyFlakeCheckers = 'pep8,mccabe,pyflakes'
let g:PyFlakeDefaultComplexity=10

let g:syntastic_python_checkers = ['pyflakes', 'pep8', 'hlint']

nnoremap <leader>r :REPLToggle<Cr>

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
