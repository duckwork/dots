" ViMrc v0.9.0
" by Case Duckworth

if filereadable(glob("~/dots/vimrc"))
  let g:myvimrc = glob("~/dots/vimrc")
else
  let g:myvimrc = $MYVIMRC
endif
let g:tw        = 78
let g:stl       = {}
let g:stl.plug  = []
let g:is_bash   = 1
let g:spelldir  = glob("~/.vim/spell")

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
set linebreak showbreak=\\
set wrap
set sidescroll=1 sidescrolloff=4
set linespace=2
set list listchars=tab:..,trail:_,extends:>,precedes:<,nbsp:~
set scrolloff=4
set display=lastline
let &synmaxcol = g:tw + 2
set laststatus=2 showtabline=2
set tabline=%!Tabline()
function! Tabline()
  let t = ''
  let nt = tabpagenr('$')
  if nt == 1
    return ''
  endif
  for i in range(nt)
    if i + 1 == tabpagenr()
      let t .= '%#TabLineSel#'
    else
      let t .= '%#TabLine#'
    endif
    let t .= ' %' . (i + 1) . 'T'
    let t .= '%{Tablabel(' . (i + 1) . ')}'
  endfor
  if nt <= 1
    let t .= '%#TabLineFill#%T'
  else
    let t .= '%#TabLine#%T'
    let t .= '%=%999X-'
  endif
  return t
endfunction
function! Tablabel(n)
  let buflist = tabpagebuflist(a:n)
  let tabnr = a:n
  let winnr = tabpagewinnr(a:n)
  let tabwins = tabpagewinnr(a:n, '$')
  let fname = fnamemodify(bufname(buflist[winnr - 1]), ':t')
  if fname == ''
    let fname = '__'
  endif
  if tabwins > 1
    return tabnr.': '.fname.' ('.winnr.'/'.tabwins.')'.' '
  else
    return tabnr.': '.fname.' '
  endif
endfunction
" ----------------------- Spelling options
set spelllang=en_us
let &spellfile = g:spelldir . "en.utf-8.add"
" ----------------------- Messages options
set confirm
set noerrorbells visualbell
set showcmd
set helplang=en
set report=5
set shortmess=aoOstTWI
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
set nrformats=hex,alpha
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
  set shellslash
  set viminfo+=rA:,rB:
  augroup WinOpts
    au! VimEnter *
          \ if getcwd() =~? "C:[\\/]WINDOWS[\\/]System32" && bufname("") == ""
          \ |  cd ~
          \ | endif
  augroup END
endif
" ----------------------- GUI options
if has('gui_running')
  if has('gui_gtk2')
    set guifont=Source\ Code\ Pro\ 9
  elseif has('gui_win32')
    set guifont=Courier_Prime_Source:h9:cANSI
  elseif has('x11')
    set guifont=*-terminus-*-*-*-*-12-*-*-*-*-*-*-*
  endif
  set guioptions=aci
  set winaltkeys=no
  augroup VimEnterGUI
    au!
    au VimEnter * let &columns = g:tw + &fdc + &nu * &nuw + 1
  augroup END
endif

" ----------------------- Keymaps
set timeoutlen=1000
let mapleader = ','
let maplocalleader = ','
noremap ; :
noremap : ;

" Basic movement
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
nnoremap H ^
nnoremap L g_

" Operators
nnoremap Y y$
nnoremap K i<CR><Esc>d^kg_lD

" Buffers
nnoremap <BS> :b#<CR>

" Windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Formatting
nnoremap Q gqip
vnoremap Q gq
nnoremap <silent> <Leader>Q :call util#keepjumps("gggqG")<CR>
nnoremap <silent> <Leader>gu :call util#fixLineEndings()<CR>

" Toggle settings
nnoremap <silent> <Leader>bg :let &bg = &bg == 'dark' ? 'light' : 'dark'<CR>
nnoremap <silent> <Leader>nr :set number!<CR>
nnoremap <silent> <Leader>/ :nohlsearch<CR>
nnoremap <silent> <Leader>rs :call util#removeEOLSpaces()<CR>

" Commands
nnoremap <F1> K

" ----------------------- Mouse
if has('mouse')
  set mouse=a
  set mousemodel=popup
endif

" ----------------------- Autocmds
augroup WindowCmds
  au!
  au FocusLost   * silent! wall
  au BufEnter    * call util#notHelp('silent! lcd %:p:h')
  au BufReadPost * normal! `"
  au BufWinLeave * silent! mkview
  au BufWinEnter * silent! loadview
  au BufReadPost * call util#updateModifiable()
  au VimEnter,WinEnter,BufWinEnter * call <SID>updateStatus()
  au WinEnter * call util#notHelp('setlocal rnu cul')
  au WinLeave * setlocal nornu nocul
augroup END
function! s:updateStatus()
  let &l:statusline = substitute(join(g:stl.left) .
                    \ ' %= ' . join(g:stl.plug) .
                    \ ' ' . join(g:stl.right) . ' ', '\s\+', ' ', 'g')
endfunction

augroup ModeCmds
  au!
  au InsertEnter * set nornu | let &cc = g:tw + 1
  au InsertLeave * call util#notHelp('set rnu cc=')
augroup END

augroup ft_Text
  au!
  au BufNewFile,BufRead *.txt call util#notHelp('setf pandoc')
  au BufNewFile,BufRead *.m.*d.*    setf markdown
  au FileType markdown,pandoc call TextMode()
  au FileType markdown,pandoc
        \nnoremap <buffer> <Leader>" :call util#unintelligent()<CR>
augroup END
function! TextMode()
  setlocal spell spelllang=en_us
  setlocal shiftwidth=4 softtabstop=4
  setlocal cpoptions+=J
  setlocal formatoptions=tnroqaw
  setlocal formatlistpat =^\\s*\\([*+-]\\\|\\((*\\d\\+[.)]\\+\\)
  setlocal formatlistpat+=\\\|\\((*\\l[.)]\\+\\)\\)\\s\\+
  exec "setlocal textwidth=".g:tw
  if executable("par")
    set formatprg=par
  endif
  let b:istext = 1
endfunction

augroup ft_Help
  au!
  au FileType help setlocal nospell
  au FileType help nnoremap <buffer> <CR> <C-]>
  au FileType help nnoremap <buffer> <BS> <C-t>
  au FileType help nnoremap <buffer> q    :q<CR>
  au BufWinEnter *.txt
        \ if &ft == 'help' && winwidth(0) >= 2 * g:tw
        \|  wincmd L
        \| endif
augroup END

augroup MyColorSchemes
  au!
  au BufWritePost minimal.vim colorscheme minimal
  au BufWritePost thatoldlook.vim colorscheme thatoldlook
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

" ----------------------- Functions
function! WordCount()
  if !exists('b:istext')
    return ''
  endif
  let oldstat = v:statusmsg
  let position = getpos('.')
  exe ":silent normal g\<c-g>"
  let status = v:statusmsg
  let wordcount = ''
  if status != '--No lines in buffer--'
    let st = split(substitute(status, ';', '', 'g'))
    if mode() !~? '[v]'
      let wordcount = st[11]
    else
      let wordcount = st[7] . '(' . st[5] . ')'
    endif
  endif
  let statusmsg = oldstat
  call setpos('.', position)
  unlet oldstat position status
  return '[' . wordcount . 'w]'
endfunction
call add(g:stl.plug, '%{WordCount()}')

function! PasteStl()
  if &paste
    return '[Paste]'
  else
    return ''
  endif
endfunction
call add(g:stl.plug, '%{PasteStl()}')

function! UniqueFT()
  if &ft =~? join(split(expand('%:e'), '\zs'), '.*')
    return ''
  else
    return ': '.&ft
  endif
endfunction

" ----------------------- Plugins
call plug#begin('~/.vim/plugged')

Plug 'Shougo/Unite.vim'              " Unite
Plug 'Shougo/neomru.vim'
Plug 'Shougo/neoyank.vim'
Plug 'bronson/vim-visual-star-search'
Plug 'dockyard/vim-easydir'
Plug 'habamax/vim-skipit'
Plug 'haya14busa/incsearch.vim'      " IncSearch
Plug 'junegunn/goyo.vim'             " Goyo
Plug 'junegunn/limelight.vim'
Plug 'kopischke/unite-spell-suggest'
Plug 'mbbill/undotree'               " Undotree
Plug 'reedes/vim-litecorrect', { 'for': [ 'pandoc', 'markdown', 'text' ] }
Plug 'shinokada/dragvisuals.vim'     " DragVisuals
Plug 'tommcdo/vim-exchange'
Plug 'tommcdo/vim-lion'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-capslock'            " Capslock
  call add(g:stl.plug, '%{CapsLockStatusline()}')
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-pandoc/vim-pandoc-syntax'  " PandocSyntax
Plug 'vim-scripts/SyntaxAttr.vim'    " SyntaxAttr
Plug 'vim-scripts/gitignore'
Plug 'vim-scripts/matchit.zip'

if has('win32') || has('win64')
  Plug 'kkoenig/wimproved.vim'
  au GUIEnter * silent! WToggleClean
  nnoremap <F11> :WToggleFullscreen<CR>:Goyo<CR>
endif
if executable('git')
  Plug 'esneider/YUNOcommit.vim'
    let g:YUNOcommit_after = 15
  Plug 'tpope/vim-fugitive'
    nnoremap <Leader>gst :w<CR>:Gstatus<CR>
    call add(g:stl.plug, '%{fugitive#statusline()}')
  Plug 'tpope/vim-git'
endif

let g:plug_url_format = 'https://github.com/%s.git' " My plugins go here
Plug 'duckwork/minimal'
Plug 'duckwork/quick-scope', {'branch': 'patch-1'}
Plug 'duckwork/vim-thatoldlook'
call plug#end()

" ----------------------- Plugin options
"Goyo
let g:goyo_width = g:tw + 1
let g:goyo_margin_top = 3
let g:goyo_margin_bottom = g:goyo_margin_top
"IncSearch
let g:incsearch#auto_nohlsearch = 1
let g:incsearch#consistent_n_direction = 1
let g:incsearch#do_not_save_error_message_history = 1
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
" xnoremap \| :EasyAlign //<Left>
" vmap <Enter> <Plug>(EasyAlign)
" nmap <Leader>a <Plug>(EasyAlign)
"DragVisuals
vmap <expr> <LEFT>  DVB_Drag('left')
vmap <expr> <RIGHT> DVB_Drag('right')
vmap <expr> <DOWN>  DVB_Drag('down')
vmap <expr> <UP>    DVB_Drag('up')
vmap <expr> D       DVB_Duplicate()
let g:DVB_TrimeWS = 1
"PandocSyntax
let g:pandoc#syntax#conceal#use = 0
"QuickScope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
"SyntaxAttr
nnoremap <silent> <F3> :call SyntaxAttr()<CR>
"Undotree
nnoremap <F5> :UndotreeToggle<CR>
"Unite
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#custom#profile('default', 'context', {
      \ 'start_insert': 1,
      \ 'winheight'   : 10,
      \ 'direction'   : 'dynamictop',
      \ })
nnoremap <silent> - :<C-u>call <SID>uniteExplore()<CR>
function! s:uniteExplore()
  let oldsearch = @/
  UniteWithBufferDir -no-split -no-start-insert file
  silent! exe "normal! /\<C-r>#\r0"
  nohlsearch
  let @/ = oldsearch
  unlet oldsearch
endfunction
nnoremap <C-p> :<C-u>Unite history/yank<CR>
nnoremap <C-Space> :<C-u>Unite buffer neomru/file file<CR>
nnoremap z= :<C-u>Unite -no-start-insert spell_suggest<CR>
hi! link uniteCandidateSourceName Comment

augroup PluginAutocmds
  au!
  au VimEnter * call <SID>exploreEmptyVim()
  au User GoyoEnter Limelight | set nonu nornu
  au User GoyoLeave Limelight! | set nu rnu
  au FileType unite call <SID>myUniteSettings()
augroup END
function! s:exploreEmptyVim()
  if bufname("") == "" && bufnr("$") == 1
    Unite -no-split neomru/file file
  endif
endfunction
function! s:myUniteSettings()
  imap <silent><buffer><expr> <C-s> unite#do_action('split')
  imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
endfunction

" ----------------------- Theming
colorscheme thatoldlook
set number relativenumber cursorline
" Statusline
let g:stl.left = [
      \ "%2n%{mode()}",
      \ "%f", "%{UniqueFT()} %M",
      \ ]
let g:stl.right = [
      \ "%-10.(%l:%c%V%)",
      \ "%P",
      \ ]
