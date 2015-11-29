" ViMrc v0.8.0
" by Case Duckworth

" ----------------------- Important options
set nocompatible
filetype plugin indent on
"syntax enable
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
set list listchars=tab:>-,trail:·,extends:>,precedes:<,eol:\ ,nbsp:~
"set wrap
set display=lastline
set scrolloff=8
set sidescroll=1
set sidescrolloff=1
" ----------------------- Command line options
set showcmd
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
if has('win32')
  set runtimepath+=$HOME\\.vim
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

" ----------------------- Mappings
set timeoutlen=1000
let mapleader = ','
let maplocalleader = ','
noremap ; :

" Basic movement
nnoremap H ^
nnoremap L g_

" Operators
nnoremap Y y$
nnoremap K 

" Windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Formatting
nnoremap Q gqip
vnoremap Q gq
nnoremap <Leader>Q mzgggqG`z
nnoremap <silent> <Leader>gu :call <SID>fixLineEndings()<CR>
function! s:fixLineEndings()
  update
  edit ++ff=dos
  setlocal ff=unix
  write
endfunction

" Toggle settings
nnoremap <silent> <Leader>nr :set number!<CR>
nnoremap <silent> <Leader>bg :call <SID>toggleBG()<CR>
function! s:toggleBG()
  if &bg == "light"
    set bg=dark
  else
    set bg=light
  endif
endfunction

" ----------------------- Mouse
if has('mouse')
  set mouse=a
  set mousemodel=popup
endif

" ----------------------- Filetype autocommands
augroup Text
augroup END
