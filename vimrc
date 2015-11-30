" ViMrc v0.8.0
" by Case Duckworth

if filereadable(glob("~/dots/vimrc"))
  let g:myvimrc = glob("~/dots/vimrc")
else
  let g:myvimrc = $MYVIMRC
endif
let g:tw      = 78
let g:is_bash = 1

" ----------------------- Important options
set nocompatible
filetype plugin indent on
syntax enable
" ----------------------- Searching options
set ignorecase smartcase
set incsearch
set hlsearch
set magic
set wrapscan
" ----------------------- Display options
set ruler
set breakindent
set lazyredraw
set linebreak
set list
set listchars=tab:..,trail:_,extends:>,precedes:<,nbsp:~
"set wrap
set display=lastline
set scrolloff=8
set sidescroll=1
set sidescrolloff=1
" ----------------------- Messages options
set confirm
set noerrorbells visualbell
set showcmd
set helplang=en
set report=5
set shortmess=aIoOtT
"set verbose=0 verbosefile=~/.vim/log.txt
" ----------------------- Command line options
set wildmenu wildignorecase wildmode=full
set history=1000
" ----------------------- Window options
set hidden
set switchbuf=useopen
set splitbelow splitright
set equalalways eadirection=both
set winheight=20 winminheight=2 winminwidth=2
set laststatus=1
" ----------------------- Editing options
set backspace=indent,eol,start
set whichwrap=b,s,<,>,[,]
set autoindent smartindent
set expandtab smarttab shiftround
set shiftwidth=2 softtabstop=2
set undolevels=10000 undoreload=10000
set undofile undodir=$HOME/.vim/undo
set gdefault
set virtualedit=block
" ----------------------- Read/write options
set autoread autowriteall
set backup backupcopy=yes backupdir=$HOME/.vim/backup
set fileformat=unix fileformats=unix,dos
set encoding=utf-8 fileencoding=utf-8
set directory=$HOME/.vim/swap
" ----------------------- Session options
set exrc secure
set viewdir=$HOME/.vim/view
set viewoptions=folds,cursor,slash,unix
set viminfo=!,'100,<50,s10,h

" ----------------------- OS options
if has('win32') || has('win64')
  set runtimepath+=$HOME\\.vim
  set viminfo+=rA:,rB:
endif
" ----------------------- GUI options
if has('gui_running')
  if has('gui_gtk2')
    set guifont=Source\ Code\ Pro\ 9
  elseif has('gui_win32')
    set guifont=InputMonoCondensed:h9:cANSI
  elseif has('x11')
    set guifont=*-terminus-*-*-*-*-12-*-*-*-*-*-*-*
  endif
  set guioptions=aci     " (a)utocopy selection,
                         " (c)onsole choices,
                         " Vim (i)con
  set winaltkeys=no      " Don't use <ALT> for menu access
endif

" ----------------------- Keymaps
set timeoutlen=1000
let mapleader = ','
let maplocalleader = ','
noremap ; :

" Basic movement
nnoremap H ^
nnoremap L g_

" Operators
nnoremap Y y$
nnoremap K i<CR><Esc>d^kg_lD

" Windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <

" Formatting
nnoremap Q gqip
vnoremap Q gq
nnoremap <Leader>Q m]gggqG`]
nnoremap <silent> <Leader>gu :call <SID>fixLineEndings()<CR>
function! s:fixLineEndings()
  update
  edit ++ff=dos
  setlocal ff=unix
  write
endfunction

" Toggle settings
nnoremap <silent> <Leader>nr :set number!<CR>
nnoremap <silent> <Leader>/ :nohlsearch<CR>
nnoremap <silent> <Leader>rs :call <SID>removeEOLSpaces()<CR>
function! s:removeEOLSpaces()
  exe 'normal! m]'
  let prevsearch = @/
  %s/\v\s+$//
  let @/ = prevsearch
  unlet prevsearch
  exe 'normal! `]'
endfunction

" ----------------------- Mouse
if has('mouse')
  set mouse=a
  set mousemodel=popup
endif

" ----------------------- Autocommands
augroup WindowCmds
  au!
  au FocusLost   * silent! wall
  au BufEnter    * if &ft != 'help'
                \|   silent! lcd %:p:h
                \| endif
  au BufReadPost * normal! `"
  au BufWinLeave * silent! mkview
  au BufWinEnter * silent! loadview
augroup END

augroup ft_Text
  au!
  au BufNewFile,BufRead *.txt
        \ if &ft != 'help' | setf pandoc | endif
  au BufNewFile,BufRead *.m.*d.*    setf markdown
  au FileType *wiki,markdown,pandoc setlocal spell
  au FileType *wiki,markdown,pandoc setlocal shiftwidth=4 softtabstop=4
  au FileType *wiki,markdown,pandoc setlocal cpoptions+=J
augroup END
augroup ft_Help
  au!
  au FileType help setlocal nospell
  au FileType help nnoremap <buffer> <CR> <C-]>
  au FileType help nnoremap <buffer> <BS> <C-t>
  au FileType help nnoremap <buffer> q    :q<CR>
augroup END

augroup MinimalScheme
  au!
  au BufWritePost minimal.vim colorscheme minimal
augroup END

" ----------------------- Cleaning up
if exists("*mkdir")
  for directory in [
        \ &directory,
        \ &backupdir,
        \ &undodir,
        \ &viewdir,
        \ ]
    if !isdirectory(expand(directory))
      call mkdir(expand(directory), 'p')
    endif
  endfor
endif

" ----------------------- Plugins
if empty(glob('~/.vim/autoload/plug.vim'))
  " TODO: make os-agnostic
  silent !mkdir -p ~/.vim/autoload
  silent !curl -fLo ~/.vim/autoload/plug.vim
  \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  au VimEnter * PlugInstall
endif
call plug#begin('~/.vim/plugged')
Plug 'vim-scripts/matchit.zip'
Plug 'vim-scripts/SyntaxAttr.vim'
  nnoremap <silent> <F3> :call SyntaxAttr()<CR>
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'gorodinskiy/vim-coloresque'
Plug 'vim-pandoc/vim-pandoc-syntax'
  let g:pandoc#syntax#conceal#use = 0
" My plugins go here
let g:plug_url_format = 'https://github.com/%s.git'
Plug 'duckwork/minimal'
call plug#end()

colorscheme minimal
set number cursorline
