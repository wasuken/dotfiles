if !has('nvim')
  set ttymouse=xterm2
endif

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

filetype plugin on
set omnifunc=syntaxcomplete#Complete

"call plug#begin()
call plug#begin(stdpath('data') . '/plugged')
  " The default plugin directory will be as follows:
  "   - Vim (Linux/macOS): '~/.vim/plugged'
  "   - Vim (Windows): '~/vimfiles/plugged'
  "   - Neovim (Linux/macOS/Windows): stdpath('data') . '/plugged'
  " You can specify a custom plugin directory by passing it as the argument
  "   - e.g. `call plug#begin('~/.vim/plugged')`
  "   - Avoid using standard Vim directory names like 'plugin'
  
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'junegunn/vim-easy-align'
  Plug 'https://github.com/junegunn/vim-github-dashboard.git'
  Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
  Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
  Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }
  Plug 'fatih/vim-go', { 'tag': '*' }
  Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'dyng/ctrlsf.vim'
  Plug 'vijaymarupudi/nvim-fzf'
  Plug 'eightpigs/win_resize.nvim'
  Plug 'mattn/emmet-vim'
  Plug 'https://github.com/preservim/nerdtree.git'
  Plug 'https://github.com/ap/vim-css-color.git'
  Plug 'jiaoshijie/undotree'
  Plug 'wuelnerdotexe/vim-astro'
  Plug 'roman/golden-ratio'
  Plug 'hylang/vim-hy'

call plug#end()

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard'] "Hide files in .gitignore
let g:ctrlp_show_hidden = 1

nnoremap <leader>u :UndotreeToggle<cr>

nmap     <C-F>f <Plug>CtrlSFPrompt
nmap     <C-F>n <Plug>CtrlSFCwordPath
nmap     <C-F>p <Plug>CtrlSFPwordPath

nnoremap <Esc><Esc> :nohlsearch<CR><ESC>
inoremap <silent> jj <ESC>
nmap <C-l> :tabnext<CR>
nmap <C-x> :tabnew<CR>
nmap fl :FZF<CR>

let g:user_emmet_mode='a'

nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsForwardTrigger="<c-n>"
let g:UltiSnipsBackwardTrigger="<c-p>"

let g:astro_typescript = 'enable'
