set nowritebackup
set nobackup 
set virtualedit=block
set backspace=indent,eol,start
set ambiwidth=double
set smartcase
set wrapscan
set incsearch
set shellslash
set showmatch matchtime=1
set cinoptions+=:0
set cmdheight=2
set laststatus=2
set showcmd
set display=lastline
set list
set listchars=tab:^\ ,trail:~
set history=10000
let mapleader = ","

hi Comment ctermfg=3
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set guioptions-=T
set guioptions+=a
set guioptions-=m
set guioptions+=R
set showmatch
set smartindent
set noswapfile
set nofoldenable
set title
set number
set clipboard=unnamed,autoselect

let g:rufo_auto_formatting = 1

nnoremap <Esc><Esc> :nohlsearch<CR><ESC>
inoremap <silent> jj <ESC>
nmap <C-l> :tabnext<CR>

nnoremap <silent> gf :e .<CR><ESC>

syntax on
filetype plugin indent on
set nrformats=
set whichwrap=b,s,h,l,<,>,[,],~
set mouse=a

inoremap { {}<LEFT>
inoremap [ []<LEFT>
inoremap ( ()<LEFT>
inoremap " ""<LEFT>
inoremap ' ''<LEFT>

inoremap <C-P> <ESC>:call PhpDocSingle()<CR>i
nnoremap <C-P> :call PhpDocSingle()<CR>
vnoremap <C-P> :call PhpDocRange()<CR>

let g:pdv_cfg_Type = "mixed"
let g:pdv_cfg_Package = ""
let g:pdv_cfg_Version = ""
let g:pdv_cfg_Author = "wasuken <wasuken1@gmail.com>"
let g:pdv_cfg_Copyright = ""
let g:pdv_cfg_License = "mit"

" vim-powerline start

" Powerline系フォントを利用する
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#whitespace#mixed_indent_algo = 1
let g:airline_theme = 'tomorrow'
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.maxlinenr = '㏑'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

" vim-powerlne end

nnoremap <silent>bp :bprevious<CR>
nnoremap <silent>bn :bnext<CR>
nnoremap <silent>bb :b#<CR>

" sneark
let g:sneak#label = 1

" ycm
let g:ycm_server_python_interpreter = '/Users/takedamasaru/.pyenv/versions/anaconda3-5.3.1/bin/python'
let g:ycm_python_binary_path = '/Users/takedamasaru/.pyenv/versions/anaconda3-5.3.1/bin/python'
let g:ycm_auto_trigger = 1
let g:ycm_min_num_of_chars_for_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
set completeopt-=preview
let g:ycm_add_preview_to_completeopt = 0

map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

augroup source-vimrc
  autocmd!
  autocmd BufWritePost *vimrc source $MYVIMRC | set foldmethod=marker
  autocmd BufWritePost *gvimrc if has('gui_running') source $MYGVIMRC
augroup ENDaugroup auto_comment_off
  autocmd!
  autocmd BufEnter * setlocal formatoptions-=r
  autocmd BufEnter * setlocal formatoptions-=o
augroup ENDaugroup MyXML
  autocmd!
  autocmd Filetype xml inoremap <buffer> </ </<C-x><C-o>
  autocmd Filetype html inoremap <buffer> </ </<C-x><C-o>
augroup END

"autopep8を<sift>+fで実行
function! Preserve(command)
  " Save the last search.
  let search = @/
  " Save the current cursor position.
  let cursor_position = getpos('.')
  " Save the current window position.
  normal! H
  let window_position = getpos('.')
  call setpos('.', cursor_position)
  " Execute the command.
  execute a:command
  " Restore the last search.
  let @/ = search
  " Restore the previous
  " window position.
  call setpos('.', window_position)
  normal! zt
  " Restore the previous cursor position.
  call setpos('.', cursor_position)
endfunction
function! Autopep8()
  call Preserve(':silent %!autopep8 --ignore=E501 -')
endfunction
                                                                        autocmd FileType python nnoremap <S-f> :call Autopep8()<CR>

let g:rspec_command = "!bundle exec rspec {spec}"
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

let s:dein_dir = expand('$HOME/.cache/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
"if &compatible
"    set nocompatible               " Be iMproved
"endif
" dein.vimがない場合githubからDL
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
    execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif
" 設定開始
if dein#load_state(s:dein_dir)
    call dein#begin(s:dein_dir)
    " プラグインリスト(toml)
    let g:rc_dir    = expand('$HOME/.vim')
    let s:toml      = g:rc_dir . '/dein.toml'
    let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'
    " tomlのロード
    call dein#load_toml(s:toml,      {'lazy':0})
    call dein#load_toml(s:lazy_toml, {'lazy':1})
    " 設定終了
    call dein#end()
    call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable
" 未インストールがあればインストール
if dein#check_install()
    call dein#install()
endif


if executable('pyls')
  " pip install python-language-server
  au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'allowlist': ['python'],
        \ })
endif

function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> gs <plug>(lsp-document-symbol-search)
  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [g <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]g <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
  inoremap <buffer> <expr><c-f> lsp#scroll(+4)
  inoremap <buffer> <expr><c-d> lsp#scroll(-4)

  let g:lsp_format_sync_timeout = 1000
  autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')

  " refer
  " to
  " doc
  " to
  " add
  " more
  " commands
endfunction

augroup lsp_install
  au!
  " call
  " s:on_lsp_buffer_enabled
  " only
  " for
  " languages
  " that
  " has
  " the
  " server
  " registered.
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

augroup MyLsp
  autocmd!
  " pip install python-language-server
  if executable('pyls')
    " Python用の設定を記載
    " workspace_configで以下の設定を記載
    " - pycodestyleの設定はALEと重複するので無効にする
    " - jediの定義ジャンプで一部無効になっている設定を有効化
    autocmd User lsp_setup call lsp#register_server({
          \ 'name': 'pyls',
          \ 'cmd': { server_info -> ['pyls'] },
          \ 'whitelist': ['python'],
          \ 'workspace_config': {'pyls': {'plugins': {
            \   'pycodestyle': {'enabled': v:false},
            \   'jedi_definition': {'follow_imports': v:true, 'follow_builtin_imports': v:true},}}}
            \})
    autocmd FileType python call s:configure_lsp()
  endif
  if executable('solargraph')
      " gem install solargraph
      au User lsp_setup call lsp#register_server({
          \ 'name': 'solargraph',
          \ 'cmd': {server_info->[&shell, &shellcmdflag, 'solargraph stdio']},
          \ 'initialization_options': {"diagnostics": "true"},
          \ 'whitelist': ['ruby'],
          \ })
  endif
  autocmd FileType ruby call s:configure_lsp()
augroup END

" 言語ごとにServerが実行されたらする設定を関数化
function! s:configure_lsp() abort
  setlocal omnifunc=lsp#complete   " オムニ補完を有効化
  " LSP用にマッピング
  nnoremap <buffer> <C-]> :<C-u>LspDefinition<CR>
  nnoremap <buffer> gd :<C-u>LspDefinition<CR>
  nnoremap <buffer> gD :<C-u>LspReferences<CR>
  nnoremap <buffer> gs :<C-u>LspDocumentSymbol<CR>
  nnoremap <buffer> gS :<C-u>LspWorkspaceSymbol<CR>
  nnoremap <buffer> gQ :<C-u>LspDocumentFormat<CR>
  vnoremap <buffer> gQ :LspDocumentRangeFormat<CR>
  nnoremap <buffer> K :<C-u>LspHover<CR>
  nnoremap <buffer> <F1> :<C-u>LspImplementation<CR>
  nnoremap <buffer> <F2> :<C-u>LspRename<CR>
endfunction
let g:lsp_diagnostics_enabled = 0

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

nmap gl :GFile<CR>

  
" Vim color file

set background=dark
highlight clear

if exists("syntax_on")
  syntax reset
endif

set t_Co=256
" let g:colors_name = "moneyforward"

hi Cursor ctermfg=235 ctermbg=231 cterm=NONE
hi Visual ctermfg=NONE ctermbg=59 cterm=NONE
hi CursorLine ctermfg=NONE ctermbg=237 cterm=NONE
hi CursorColumn ctermfg=NONE ctermbg=237 cterm=NONE
hi ColorColumn ctermfg=NONE ctermbg=237 cterm=NONE
hi LineNr ctermfg=102 ctermbg=237 cterm=NONE
hi VertSplit ctermfg=241 ctermbg=241 cterm=NONE
hi MatchParen ctermfg=208 ctermbg=NONE cterm=underline
hi StatusLine ctermfg=231 ctermbg=241 cterm=bold
hi StatusLineNC ctermfg=231 ctermbg=241 cterm=NONE
hi Pmenu ctermfg=NONE ctermbg=NONE cterm=NONE
hi PmenuSel ctermfg=NONE ctermbg=59 cterm=NONE
hi IncSearch ctermfg=235 ctermbg=81 cterm=NONE
hi Search ctermfg=NONE ctermbg=NONE cterm=underline
hi Directory ctermfg=130 ctermbg=NONE cterm=NONE
hi Folded ctermfg=242 ctermbg=235 cterm=NONE
hi SignColumn ctermfg=NONE ctermbg=237 cterm=NONE
hi Normal ctermfg=231 ctermbg=235 cterm=NONE
hi Boolean ctermfg=130 ctermbg=NONE cterm=NONE
hi Character ctermfg=130 ctermbg=NONE cterm=NONE
hi Comment ctermfg=242 ctermbg=NONE cterm=NONE
hi Conditional ctermfg=208 ctermbg=NONE cterm=NONE
hi Constant ctermfg=NONE ctermbg=NONE cterm=NONE
hi Define ctermfg=208 ctermbg=NONE cterm=NONE
hi DiffAdd ctermfg=231 ctermbg=64 cterm=bold
hi DiffDelete ctermfg=88 ctermbg=NONE cterm=NONE
hi DiffChange ctermfg=NONE ctermbg=NONE cterm=NONE
hi DiffText ctermfg=231 ctermbg=24 cterm=bold
hi ErrorMsg ctermfg=231 ctermbg=208 cterm=NONE
hi WarningMsg ctermfg=231 ctermbg=208 cterm=NONE
hi Float ctermfg=130 ctermbg=NONE cterm=NONE
hi Function ctermfg=214 ctermbg=NONE cterm=NONE
hi Identifier ctermfg=32 ctermbg=NONE cterm=NONE
hi Keyword ctermfg=208 ctermbg=NONE cterm=NONE
hi Label ctermfg=81 ctermbg=NONE cterm=NONE
hi NonText ctermfg=59 ctermbg=236 cterm=NONE
hi Number ctermfg=130 ctermbg=NONE cterm=NONE
hi Operator ctermfg=208 ctermbg=NONE cterm=NONE
hi PreProc ctermfg=208 ctermbg=NONE cterm=NONE
hi Special ctermfg=231 ctermbg=NONE cterm=NONE
hi SpecialComment ctermfg=242 ctermbg=NONE cterm=NONE
hi SpecialKey ctermfg=59 ctermbg=237 cterm=NONE
hi Statement ctermfg=208 ctermbg=NONE cterm=NONE
hi StorageClass ctermfg=32 ctermbg=NONE cterm=NONE
hi String ctermfg=81 ctermbg=NONE cterm=NONE
hi Tag ctermfg=208 ctermbg=NONE cterm=NONE
hi Title ctermfg=231 ctermbg=NONE cterm=bold
hi Todo ctermfg=95 ctermbg=NONE cterm=inverse,bold
hi Type ctermfg=208 ctermbg=NONE cterm=NONE
hi Underlined ctermfg=NONE ctermbg=NONE cterm=underline
hi rubyClass ctermfg=208 ctermbg=NONE cterm=NONE
hi rubyFunction ctermfg=214 ctermbg=NONE cterm=NONE
hi rubyInterpolationDelimiter ctermfg=NONE ctermbg=NONE cterm=NONE
hi rubySymbol ctermfg=130 ctermbg=NONE cterm=NONE
hi rubyConstant ctermfg=32 ctermbg=NONE cterm=NONE
hi rubyStringDelimiter ctermfg=81 ctermbg=NONE cterm=NONE
hi rubyBlockParameter ctermfg=208 ctermbg=NONE cterm=NONE
hi rubyInstanceVariable ctermfg=NONE ctermbg=NONE cterm=NONE
hi rubyInclude ctermfg=208 ctermbg=NONE cterm=NONE
hi rubyGlobalVariable ctermfg=NONE ctermbg=NONE cterm=NONE
hi rubyRegexp ctermfg=81 ctermbg=NONE cterm=NONE
hi rubyRegexpDelimiter ctermfg=81 ctermbg=NONE cterm=NONE
hi rubyEscape ctermfg=130 ctermbg=NONE cterm=NONE
hi rubyControl ctermfg=208 ctermbg=NONE cterm=NONE
hi rubyClassVariable ctermfg=NONE ctermbg=NONE cterm=NONE
hi rubyOperator ctermfg=208 ctermbg=NONE cterm=NONE
hi rubyException ctermfg=208 ctermbg=NONE cterm=NONE
hi rubyPseudoVariable ctermfg=NONE ctermbg=NONE cterm=NONE
hi rubyRailsUserClass ctermfg=32 ctermbg=NONE cterm=NONE
hi rubyRailsARAssociationMethod ctermfg=32 ctermbg=NONE cterm=NONE
hi rubyRailsARMethod ctermfg=32 ctermbg=NONE cterm=NONE
hi rubyRailsRenderMethod ctermfg=32 ctermbg=NONE cterm=NONE
hi rubyRailsMethod ctermfg=32 ctermbg=NONE cterm=NONE
hi erubyDelimiter ctermfg=NONE ctermbg=NONE cterm=NONE
hi erubyComment ctermfg=95 ctermbg=NONE cterm=NONE
hi erubyRailsMethod ctermfg=32 ctermbg=NONE cterm=NONE
hi htmlTag ctermfg=214 ctermbg=NONE cterm=NONE
hi htmlEndTag ctermfg=214 ctermbg=NONE cterm=NONE
hi htmlTagName ctermfg=NONE ctermbg=NONE cterm=NONE
hi htmlArg ctermfg=NONE ctermbg=NONE cterm=NONE
hi htmlSpecialChar ctermfg=130 ctermbg=NONE cterm=NONE
hi javaScriptFunction ctermfg=32 ctermbg=NONE cterm=NONE
hi javaScriptRailsFunction ctermfg=32 ctermbg=NONE cterm=NONE
hi javaScriptBraces ctermfg=NONE ctermbg=NONE cterm=NONE
hi yamlKey ctermfg=208 ctermbg=NONE cterm=NONE
hi yamlAnchor ctermfg=NONE ctermbg=NONE cterm=NONE
hi yamlAlias ctermfg=NONE ctermbg=NONE cterm=NONE
hi yamlDocumentHeader ctermfg=81 ctermbg=NONE cterm=NONE
hi cssURL ctermfg=208 ctermbg=NONE cterm=NONE
hi cssFunctionName ctermfg=32 ctermbg=NONE cterm=NONE
hi cssColor ctermfg=130 ctermbg=NONE cterm=NONE
hi cssPseudoClassId ctermfg=214 ctermbg=NONE cterm=NONE
hi cssClassName ctermfg=214 ctermbg=NONE cterm=NONE
hi cssValueLength ctermfg=130 ctermbg=NONE cterm=NONE
hi cssCommonAttr ctermfg=32 ctermbg=NONE cterm=NONE
hi cssBraces ctermfg=NONE ctermbg=NONE cterm=NONE
