" ViMrc v0.9.0
" by Case Duckworth

if filereadable(glob("~/dots/vimrc"))
  let g:myvimrc = glob("~/dots/vimrc")
else
  let g:myvimrc = $MYVIMRC
endif
let g:tw       = 78
let g:is_bash  = 1
let g:spelldir = glob("~/.vim/spell")

" ----------------------- Important options
set nocompatible
filetype plugin indent on
syntax enable
" ----------------------- Searching options
set ignorecase smartcase
set incsearch hlsearch
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
let &synmaxcol = g:tw + 1
" ----------------------- Spelling options
set spelllang=en_us
let &spellfile = g:spelldir . "en.utf-8.add"
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
set statusline=\ %f\ %y\ %m%=%3l:%02v\ %2p%%
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
  set guioptions=aci
  set winaltkeys=no
  augroup VimEnterGUI
    au!
    au VimEnter * let &columns = g:tw + &fdc + &nu * &nuw
  augroup END
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

nnoremap <silent> - :Explore<CR>

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
  normal! m]
  let prevsearch = @/
  silent! %s/\v\s+$//
  let @/ = prevsearch
  unlet prevsearch
  normal! `]
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
  au FileType *wiki,markdown,pandoc setlocal stl+=\ (%{WordCount()}w)
  au FileType *wiki,markdown,pandoc
        \nnoremap <buffer> <Leader>" :call <SID>unintelligent()<CR>
augroup END
function! WordCount()
  let oldstat = v:statusmsg
  let position = getpos('.')
  exe ":silent normal g\<c-g>"
  let status = v:statusmsg
  let wordcount = ''
  if status != '--No lines in buffer--' && mode() !~? '[v]'
    let wordcount = str2nr(split(status)[11])
  endif
  let statusmsg = oldstat
  call setpos('.', position)
  unlet oldstat position status
  return wordcount
endfunction
function! s:unintelligent()
  let gdef_save  = &gdefault
  set nogdefault
  let oldsearch = @/
  normal!  m]
  %sm/[‘’]/'/ge
  %sm/[“”]/"/ge
  %sm/—/---/ge
  %sm/–/--/ge
  normal! `]
  let &gdefault = gdef_save
  let @/ = oldsearch
  unlet gdef_save oldsearch
endfunction

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
        \ g:spelldir,
        \ ]
    if !isdirectory(expand(directory))
      call mkdir(expand(directory), 'p')
    endif
  endfor
endif

" ----------------------- Plugins
call plug#begin('~/.vim/plugged') " Third-party plugins go here
Plug 'dockyard/vim-easydir'
Plug 'habamax/vim-skipit'
Plug 'haya14busa/incsearch.vim' "IncSearch
Plug 'junegunn/vim-easy-align' "EasyAlign
Plug 'reedes/vim-litecorect', { 'for': [ 'pandoc', 'markdown', 'text' ] }
Plug 'Shougo/Unite.vim' "Unite
Plug 'Shougo/neomru.vim'
Plug 'Shougo/neoyank.vim'
Plug 'shinokada/dragvisuals.vim' "DragVisuals
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-capslock'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-pandoc/vim-pandoc-syntax' "PandocSyntax
Plug 'vim-scripts/SyntaxAttr.vim' "SyntaxAttr
Plug 'vim-scripts/gitignore'
Plug 'vim-scripts/matchit.zip'
if has('win32') || has('win64')
  Plug 'kkoenig/wimproved.vim'
  autocmd GUIEnter * silent! WToggleClean
  nnoremap <F11> :WToggleFullscreen<CR>
endif
let g:plug_url_format = 'https://github.com/%s.git' " My plugins go here
Plug 'duckwork/minimal'
call plug#end()

" ----------------------- Plugin options
"IncSearch
let g:incsearch#auto_nohlsearch = 1
let g:incsearch#consistent_n_direction = 1
let g:incsearch#do_not_save_error_message_history = 1
let g:incsearch#separate_highlight = 1
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)
"EasyAlign
xnoremap \| :EasyAlign //<Left>
vmap <Enter> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
"DragVisuals
vmap <expr> <LEFT>  DVB_Drag('left')
vmap <expr> <RIGHT> DVB_Drag('right')
vmap <expr> <DOWN>  DVB_Drag('down')
vmap <expr> <UP>    DVB_Drag('up')
vmap <expr> D       DVB_Duplicate()
let g:DVB_TrimeWS = 1
"PandocSyntax
let g:pandoc#syntax#conceal#use = 0
"SyntaxAttr
nnoremap <silent> <F3> :call SyntaxAttr()<CR>
"Unite
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#custom#profile('default', 'context', { 
      \ 'start_insert': 1,
      \ 'winheight': 10,
      \ 'direction': 'botright',
      \ })
nnoremap go :<C-u>Unite -start-insert neomru/file<CR>
nnoremap gy :<C-u>Unite history/yank<CR>
nnoremap <C-Space> :<C-u>Unite buffer neomru/file file history/yank<CR>

" ----------------------- Theming
colorscheme minimal
set number cursorline
