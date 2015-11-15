" ViMrc v0.8.0
" by Case Duckworth
" VARIABLES {{{ ==========================================================
if filereadable(glob("~/dots/vimrc"))
  let g:myvimrc = glob("~/dots/vimrc")
else
  let g:myvimrc = $MYVIMRC
endif
let g:tw       = 78 " fake textwidth
let g:tabwidth = 4 " how wide are <TAB>s
let g:is_bash  = 1
let g:spelldir = $HOME . ".vim/spell/"
" }}} ========================================================================
" SETTINGS {{{ ===========================================================
"  1 important {{{ -----------------------------------------------------------
set nocompatible " be iMproved
set cpoptions+=J " sentences are delimeted by 2 spaces
" }}} ------------------------------------------------------------------------
"  2 moving around, searching and patterns {{{ -------------------------------
set ignorecase    " Ignore case when searching
set incsearch     " Show match for partly typed search
set magic         " Backslashes quote more in regexes
set nostartofline " Move to the start of line with movement
set smartcase     " ... unless the pattern has upper case
set wrapscan      " Searches wrap around file ends
"set cdpath=,,
" The following keys wrap movement around lines in the given modes:
set whichwrap=b,s " <BS> and <Space>
set ww      +=<,> " Arrow keys {N, V}
set ww      +=[,] " Arrow keys {I, R}
"  }}} -----------------------------------------------------------------------
"  3 tags {{{ ----------------------------------------------------------------
"  TODO
"  }}} -----------------------------------------------------------------------
"  4 displaying text {{{ -----------------------------------------------------
set breakindent                 " preserve indents in wrapped text
set lazyredraw                  " don't redraw while executing macros
set linebreak                   " wrap long lines to a character in 'breakat'
set list
set number
"set norelativenumber
set wrap                        " long lines wrap
set cmdheight=1                 " number of lines to use for the command-line
set display=lastline            " show as much of the last line as possible
set numberwidth=4               " minimum width of line number column
set scrolloff=8                 " number of lines to show around cursor
set sidescroll=1                " minimal number of columns to scroll
set sidescrolloff=1             " number of columns to show around cursor
let &showbreak = '└ '
" Characters to fill empty space in the following lines:
let &fillchars = 'stl:.'        " Focused statusline
let &fcs      .= ',stlnc: '     " Unfocused statusline
let &fcs      .= ',vert:|'      " Vertical window separators
let &fcs      .= ',fold:~'      " 'foldtext'
let &fcs      .= ',diff:-'      " deleted lines of 'diff' option
" Characters to replace non-printing characters with:
let &listchars = 'tab:» '       " <Tab>s
let &lcs      .= ',trail:·'     " Trailing whitespace
let &lcs      .= ',extends: '   " 'nowrap': line goes past right
let &lcs      .= ',precedes: '  " 'nowrap': line goes past left
let &lcs      .= ',eol: '       " end-of-line
let &lcs      .= ',nbsp:~'      " Non-breaking spaces
"  }}} -----------------------------------------------------------------------
"  5 syntax, highlighting and spelling {{{ -----------------------------------
set cursorline                              " hilite the screen line of crsor
set hlsearch                                " highlight matches for last srch
set nocursorcolumn                          " hilite screen column of crsr
"set background=dark                         " background color brightness
let &spellfile = g:spelldir . "en.utf-8.add"  " file to store custom words
set spelllang=en_us                         " list of accepted languages
let &synmaxcol = g:tw + 3                   " maximum col to hilite to
let &colorcolumn = g:tw + 1                 " columns to hilite
"  }}} -----------------------------------------------------------------------
"  6 multiple windows {{{ ----------------------------------------------------
set equalalways           " resize windows to accomodate changes
set hidden                " don't unload a buffer when window closes
set splitbelow            " new windows open below current
set splitright            " new windoes open to the right of current
set eadirection=both      " which direction to 'equalalways'
set laststatus=2          " always show statusline
"set statusline= "__UpdateStatus()
set switchbuf=useopen     " how to switch to a buffer
set winheight=20          " minimal lines for current window
set winminheight=2        " minimum lines for any window
set winminwidth=2         " minimum columns for any window
let &winwidth = g:tw + 1  " minimal columns for current window
"  }}} -----------------------------------------------------------------------
"  7 multiple tab pages {{{ --------------------------------------------------
"set guitablable=
"set guitabtooltip=
"set showtabline=1
"set tabline=
"set tabpagemax=10
"  }}} -----------------------------------------------------------------------
"  8 terminal {{{ ------------------------------------------------------------
"set esckeys
set guicursor=n:block-Cursor/lCursor
set gcr     +=v:ver20-Cursor/lCursor-blinkon0
set gcr     +=o:hor50-Cursor/lCursor
set gcr     +=i-ci:ver20-Cursor/lCursor
set gcr     +=r-cr:hor10-Cursor/lCursor
set gcr     +=c:block-Cursor/lCursor
"set icon
"set iconstring=
"set nottyfast
"set noweirdinvert
"set scrolljump=1
"set term=$TERM
"set titlelen=85
"set titleold=Thanks\ for\ flying\ Vim
"set ttybuiltin
"set ttyscroll=999
"set ttytype=$TERM
set title
"  }}} -----------------------------------------------------------------------
"  9 using the mouse {{{ -----------------------------------------------------
if has('mouse')
  "set mousehide
  "set mouseshape=...
  "set mousetime=500
  "set nomousefocus
  "set ttymouse=
  set mouse=a          " enable mouse
  set mousemodel=popup " what the right mouse button is for
endif
"  }}} -----------------------------------------------------------------------
" 10 GUI {{{ -----------------------------------------------------------------
if has('gui_running')
  if has('gui_gtk2')
    " set guifont=Terminus\ 9 " same as termite
    set guifont=Source\ Code\ Pro\ 9
  elseif has('x11')
    set guifont=-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*
  elseif has('gui_win32')
    set guifont=Source_Code_Pro:h9:cANSI
  endif
  set guioptions=a      " autocopy Vim's selection
  set go       +=c      " use console instead of popups for choices
  set go       +=g      " grey out inactive menu items
  set go       +=i      " use Vim icon
  set go       +=t      " include tearoff menus
  set browsedir=buffer  " where to open the file browser
  set linespace=0
  set winaltkeys=no     " how to map the <ALT> keys to the menu
endif
" }}} ------------------------------------------------------------------------
" 11 printing {{{ ------------------------------------------------------------
"set printdevice=
"set printencoding=
"set printexpr=...
"set printfont=courier
"set printheader=%<%f%h%m%=Page\ %N
"set printmbcharset=
"set printmbfont=
"set printoptions=
" }}} ------------------------------------------------------------------------
" 12 messages and info {{{ ---------------------------------------------------
set confirm " ask to save before quitting
set noerrorbells " ring the bell for error messages
set ruler " show ruler position in statusline
set showcmd " show partial commands as they're typed
set noshowmode " show current mode in statusline
set visualbell " use a visual bell instead of sound
set helplang=en
set report=5 " threshold for reporting number of changed lines
set shortmess=a  " shm=filmnrwx
set shm     +=I  " no intro message
set shm     +=oO " overwrite file messages
set shm     +=tT " truncate messages in {C}
set verbose=0
set verbosefile=~/.vim/log.txt
" }}} ------------------------------------------------------------------------
" 13 selecting text {{{ ------------------------------------------------------
if has('unnamedplus')
  set clipboard=autoselect,unnamedplus,exclude:cons\\\|linux
else
  set clipboard=autoselect,unnamed,exclude:cons\\\|linux
endif
set keymodel=startsel,stopsel  " select with <Home>,<End>,<PgUp>,<PgDn>
set selection=exclusive        " how selecting text behaves
" }}} ------------------------------------------------------------------------
" 14 editing text {{{ --------------------------------------------------------
set tildeop            " make <~> command behave like an operator
"set matchpairs=(:),{:},[:]
set backspace=indent   " allow backspacing over autoindent
set bs      +=eol      " allow backspacing over line breaks
set bs      +=start    " allow backspacing over start of insert
set formatoptions-=ro  " disable autocomments in {I}
set nrformats=alpha    " inc/dec alphabetical chars w/<C-a>,<C-x>
set nf      +=hex      " ''      hexadecimal nums   ''
set textwidth=0        " where to break a line (0 -> don't)
set undolevels=10000   " max number of undoable changes
set undoreload=10000   " max lines to save for undo on reload
" }}} ------------------------------------------------------------------------
" 15 tabs and indenting {{{ --------------------------------------------------
set autoindent                 " automatically set the indent of a new line
set expandtab                  " expand <Tab> to spaces in {I}
set shiftround                 " round to 'shiftwidth' for <<,>>
set nosmartindent              " clever autoindenting [NOT ACTUALLY CLEVER]
set smarttab                   " a <Tab> in an indent inserts 'shiftwidth' spaces
let &shiftwidth = g:tabwidth   " number of spaces to autoindent
let &softtabstop = g:tabwidth  " number of spaces to a <Tab>
" }}} ------------------------------------------------------------------------
" 16 folding {{{ -------------------------------------------------------------
set foldenable
" set foldcolumn=1 " width of gutter column to indicate folds
set foldtext=FoldLine()
"set foldclose=all
"set foldexpr=0
"set foldignore=#
"set foldmarker={{{,}}}
"set foldmethod=...
"set foldminlines=1
"set foldnestmax=20
"set foldopen=block,hor,mark,percent,quickfix,search,tag,undo
" }}} ------------------------------------------------------------------------
" 17 diff mode {{{ -----------------------------------------------------------
"set diffexpr=
"set diffopt=filler
"set nodiff
"set patchexpr=
" }}} ------------------------------------------------------------------------
" 18 mapping {{{ -------------------------------------------------------------
set timeoutlen=1000
"set maxmapdepth=1000
"set remap
"set timeout
"set ttimeout
"set ttimeoutlen=-1
" }}} ------------------------------------------------------------------------
" 19 reading and writing files {{{ -------------------------------------------
set autoread                      " automatically read a file when modified outside Vim
set autowriteall
set backup
set backupcopy=yes                " whether to backup a copy or rename existing
set backupdir=$HOME/.vim/backup/  " directory to store backups
set fileformat=unix               " end-of-line format
set fileformats=unix,dos          " list of eol formats to look for
" }}} ------------------------------------------------------------------------
" 20 the swap file {{{ -------------------------------------------------------
set directory=$HOME/.vim/swap/ " list of dirs for swap files
"set swapfile
" }}} ------------------------------------------------------------------------
" 21 command line editing {{{ ------------------------------------------------
"set nofileignorecase
set undofile
set wildcharm=<C-z>
set wildignorecase        " ignore case when completing filenames
set wildmenu              " show a list of matches w/ cmd completion
set history=1000          " how many command lines are remembered
set undodir=$HOME/.vim/undoes/
" set wildignore+=*/.git/*  " ignore git files in wildmenu
" set wig       +=*/.hg/*   " ''     hg files ''
" set wig       +=*/.svn/*  " ''    svn files ''
set wildmode=full
" }}} ------------------------------------------------------------------------
" 22 executing external commands {{{ -----------------------------------------
"set shell=__
"set shellquote=
"set shellxquote=
"set shellxescape=
"set shellcmdflag=-c
"set shellredir=>%s\ 2>&1
"set shelltmp
"set equalprg=__FILETYPE__
"set formatprg=__FILETYPE__
"set keywordprg=__FILETYPE__
"set warn
" }}} ------------------------------------------------------------------------
" 23 running make and jumping to errors {{{ ----------------------------------
"set errorfile=errors.err
"set errorformat=...
"set makeprg=__FILETYPE__
"set shellpipe=2>&1\|\ tee
"set makeef=
"set grepprg=__PLATFORM__
"set grepformat=__PLATFORM__
" }}} ------------------------------------------------------------------------
" 24 language specific {{{ ---------------------------------------------------
"set isfname=..
"set isident=..
"set iskeyword=..
"set isprint=..
"set quoteescape=\\
"set norightleft
"set rightleftcmd=search
"set norevins
"set noallowrevins
"set aleph=224
"set nohkmap
"set nohkmapp
"set noaltkeymap
"set nofkmap
"set noarabic
"set arabicshape
"set notermbidi
"set keymap=
"set langmap=
"set iminsert=0
"set imsearch=0
" }}} ------------------------------------------------------------------------
" 25 multi-byte characters {{{ -----------------------------------------------
set encoding=utf-8     " character encoding used
set fileencoding=utf-8 " character encoding for file
"set fileencodings=...
"set termencoding=utf-8
"set charconvert=
"set nodelcombine
"set maxcombine=2
"set ambiwidth=single
" }}} ------------------------------------------------------------------------
" 26 various {{{ -------------------------------------------------------------
set exrc                " enable reading .vimrc in current directory
set gdefault            " use the 'g' flag by default in :s
set secure              " ..but don't let them do anything dangerous
set viewdir=$HOME/.vim/views/
set viewoptions=folds   " save folds in view
set vop       +=cursor  " save cursor position
set vop       +=slash   " save backslashes as forward slashes
set vop       +=unix    " save views with Unix eol format
set viminfo='100        " Save marks for 100 files max
set vi    ^=!           " Save global all-caps variables
set vi    +=<50         " Save 50 lines of registers
set vi    +=s10         " Save only first 10 Kb of each register
set vi    +=h           " Disable 'hlsearch' on saved files
set virtualedit=block   " allow positioning cursor anywhere in {^V}
" }}} ------------------------------------------------------------------------
" }}} ========================================================================
" KEYMAPS {{{ ============================================================
" Leaders
let mapleader = ","
let maplocalleader = ","

" Changing modes
noremap ; :
let g:jkesc = 1
if get(g:, "jkesc", 0)
  inoremap jk <Esc>
endif

" Basic movement
nnoremap <silent> j :<C-u>call LineMotion("j")<CR>
nnoremap <silent> k :<C-u>call LineMotion("k")<CR>
noremap H ^
noremap L g_
nnoremap gH :call RealScrollTo('top')<CR>
nnoremap gM M
nnoremap gL :call RealScrollTo('bot')<CR>

nnoremap ' `
nnoremap ` '

" Textobjects
" " Next - Last (Previous)
onoremap an :<C-u>call <SID>NextTextObject('a', 'f')<CR>
xnoremap an :<C-u>call <SID>NextTextObject('a', 'f')<CR>
onoremap in :<C-u>call <SID>NextTextObject('i', 'f')<CR>
xnoremap in :<C-u>call <SID>NextTextObject('i', 'f')<CR>

onoremap al :<C-u>call <SID>NextTextObject('a', 'F')<CR>
xnoremap al :<C-u>call <SID>NextTextObject('a', 'F')<CR>
onoremap il :<C-u>call <SID>NextTextObject('i', 'F')<CR>
xnoremap il :<C-u>call <SID>NextTextObject('i', 'F')<CR>

" Operators
nnoremap Y y$
nnoremap S d0

xnoremap <Leader>> gv>
xnoremap <Leader>< gv<

nnoremap gs :%s/
xnoremap gs :s/

" Window management
nnoremap <C-k>     <C-w>k
nnoremap <C-j>     <C-w>j
nnoremap <C-h>     <C-w>h
nnoremap <C-l>     <C-w>l
nnoremap <S-UP>    <C-w>K
nnoremap <S-DOWN>  <C-w>J
nnoremap <S-LEFT>  <C-w>H
nnoremap <S-RIGHT> <C-w>L

inoremap <C-Tab> <C-o>gt

nnoremap <silent> <BS> :b#<CR>

" Folds
nnoremap <Space> za
nnoremap <S-Space> zA
" TODO: toggle foldopen & close b/w setting and 'all'
" -- will need a Toggle function. -- GENERALIZE?!?!?!?

" File & filesystem shortcuts
nnoremap <Leader>er :call PopOpen(g:myvimrc)<CR>
nnoremap <Leader>re :source <C-r>=g:myvimrc<CR><CR>

nnoremap <Leader>cd :cd %:p:h<CR>
nnoremap <silent> - :Explore<CR>

if executable('sudo')
  cmap w!! %!sudo tee > /dev/null %
endif

" Remove annoyances
nnoremap <silent> <Leader>/ :nohlsearch<CR>
nnoremap <silent> <leader>rs mz:%s/\s\+$//<CR>:let @/=''<CR>`z
nnoremap <silent> <leader>rb mz:g/^$/d<CR>:let @/=''<CR>`z

" File formatting
nnoremap Q gqip
vnoremap Q gq
nnoremap <Leader>Q mzgggqG`z
nnoremap <Leader>gu :update<CR>:e ++ff=dos<CR>:setlocal ff=unix<CR>:w<CR>

nnoremap <Leader>= :call CharToEnd("=")<CR>
nnoremap <Leader>- :call CharToEnd("-")<CR>

" Toggle settings
nnoremap <silent> <Leader>bg :call ToggleBG()<CR>
nnoremap <silent> <Leader>nr :set invrelativenumber<CR>

" Clipboard
if has('unnamedplus')
  nnoremap <silent> <Leader>p :set paste<CR>"+p:set nopaste<CR>
  nnoremap <silent> <Leader>P :set paste<CR>"+P:set nopaste<CR>
else
  nnoremap <silent> <Leader>p :set paste<CR>"*p:set nopaste<CR>
  nnoremap <silent> <Leader>P :set paste<CR>"*P:set nopaste<CR>
endif

" Smarter commandline
" cnoremap <expr> <Tab> getcmdtype() == "/" \|\| getcmdtype() == "?"
"       \ ? "<CR>/<C-r>/"
"       \ : "<C-z>"
" cnoremap <expr> <S-Tab> getcmdtype() == "/" \|\| getcmdtype() == "?"
"       \ ? "<CR>?<C-r>/"
"       \ : "<S-Tab>"

" Function keys
nnoremap <F1> K
"nnoremap <F2>
"nnoremap <F3>
"nnoremap <F4>
"nnoremap <F5>
"nnoremap <F6>
"nnoremap <F7>
"nnoremap <F8>
"nnoremap <F9>
"nnoremap <F10>
"nnoremap <F11> :Goyo<CR>
nnoremap <F12> :call CloseBufWin()<CR>
" }}} ========================================================================
" FUNCTIONS {{{ ==========================================================
function! StatusLine(winnr) " {{{
  let status = ''
  " Variables {{{
  let buffer = winbufnr(a:winnr)
  let buf    = {
        \ 'name': bufname(buffer),
        \ 'dir':  expand('#'.buffer.':p:h'),
        \ 'type': getbufvar(buffer, '&filetype'),
        \ 'foc':  winnr() == a:winnr,
        \ }
  if buf.type == 'help'
    let buf.status = 'help'
  elseif getbufvar(buffer, '&readonly')
    let buf.status = 'readonly'
  elseif getbufvar(buffer, '&modified')
    let buf.status = 'modified'
  else
    let buf.status = 'normal'
  endif

  let clr = {
        \ 'def': '%#StatusLine#',
        \ 'foc': '%#CursorLineNr#',
        \ 'mut': '%#Folded#',
        \ }
  let clr.good = get(g:, 'gitgutter_enabled', 0)
                        \ ? '%#GitGutterAdd#'
                        \ : '%#DiffAdd# '
  let clr.warn = get(g:, 'gitgutter_enabled', 0)
                        \ ? '%#GitGutterDelete#'
                        \ : '%#DiffDelete# '
  let clr.info = get(g:, 'gitgutter_enabled', 0)
                        \ ? '%#GitGutterChange#'
                        \ : '%#DiffChange# '
  let spacer = clr.def . ' '
  " /vars }}}
  " Mode {{{
  let modedict = {
        \ 'n':  { 'symbol': 'λ', 'color': clr.foc },
        \ 'v':  { 'symbol': '†', 'color': clr.warn },
        \ 'V':  { 'symbol': '†', 'color': clr.warn },
        \ '': { 'symbol': '‡', 'color': clr.warn },
        \ 's':  { 'symbol': '§', 'color': clr.warn },
        \ 'S':  { 'symbol': '§', 'color': clr.warn },
        \ '': { 'symbol': '§', 'color': clr.warn },
        \ 'i':  { 'symbol': 'ι', 'color': clr.info },
        \ 'R':  { 'symbol': '∂', 'color': clr.foc },
        \ 'c':  { 'symbol': 'λ', 'color': clr.good },
        \ 'r':  { 'symbol': 'λ', 'color': clr.foc },
        \ '!':  { 'symbol': 'λ', 'color': clr.warn },
        \ }
  if buf.foc
    let status .= modedict[mode()].color . modedict[mode()].symbol
  else
    let status .= 'λ'
  endif
  let status .= spacer
  " /mode }}}
  " File information {{{
  let f = {}
  let f.name = empty(buf.name) ? '_' : buf.name
  let f.type = empty(buf.type) ? ''  : spacer . '∷ ' . buf.type

  if buf.foc
    let status .= clr.foc
    if buf.type == 'netrw'
      let status .= buf.dir
    else
      let status .= f.name . f.type
    endif
  else
    let status .= f.name
  endif
  let status .= spacer
  " /file info }}}
  " File modification status {{{
  let modifydict = {
        \ 'help': { 'symbol': '?', 'color': clr.info },
        \ 'readonly': { 'symbol': '!', 'color': clr.warn },
        \ 'modified': { 'symbol': '+', 'color': clr.good },
        \ 'normal': { 'symbol': '-', 'color': clr.foc },
        \ }
  if buf.foc
    let status .= modifydict[buf.status].color
    let status .= modifydict[buf.status].symbol . '>'
  else
    let status .= '[' . modifydict[buf.status].symbol . ']'
  endif
  let status .= spacer
  " /modifications }}}
  " Directory information {{{
  if (buf.type != 'help') && (buf.type != 'netrw') && buf.foc
    if (winwidth(a:winnr)*100) / len(buf.dir) > 500
      let status .= buf.dir
      let status .= spacer
    endif
  endif
  " /directory }}}
  " Git status {{{
  if exists('*fugitive#head')
    let head = fugitive#head()
    if empty(head) && exists('*fugitive#detect') && !exists('b:git_dir')
      call fugitive#detect(buf.dir)
      let head = fugitive#head()
    endif
  endif

  if !empty(head) && buf.foc
    let status .= clr.info
    let status .= '± ' . head
    let status .= spacer
  endif
  " /git }}}
  let status .= '%=' " Gutter
  " Toggle switches {{{
  if buf.foc
    let status .= clr.info
    if &paste
      let status .= ' ¶'
    endif
    if get(b:, 'capslock', 0)
      let status .= ' ⇑'
    endif
  endif
  let status .= spacer
  " /toggles }}}
  " Auxiliary rulers {{{
  if buf.foc
    let status .= clr.mut . '| '
    if exists('b:texty') && !(buf.type == 'help')
      let status .= '%{WordCount()} | '
    endif
    let status .= '%2p%% |'
  endif
  let status .= spacer
  " /aux rulers }}}
  " Ruler {{{
  if buf.foc
    let status .= clr.foc
    let status .= '%3l' . ':' . '%02v'
  else
    let status .= '[#%n]'
  endif
  " /ruler }}}
  return status . ' '
endfunction " }}}
function! s:RefreshStatus(...) " {{{
  if !a:0
    for nr in range(1, winnr('$'))
      call setwinvar(nr, '&statusline', '%!StatusLine('.nr.')')
    endfor
  else
    for nr in range(1, winnr('$'))
      call setwinvar(nr, '&statusline', '')
    endfor
  endif
endfunction " }}}
function! FoldLine() " {{{
    let line = getline(v:foldstart)
    let foldedlinecount = printf('%d', v:foldend - v:foldstart)
    if &foldmethod == 'marker'
        let foldmarks = substitute(&fmr, ',.*', '', '')
        let line = substitute(line, foldmarks . '\d*', '', '')
    endif
    " Get rid of commentstring
    let commentstr = substitute(&cms, '^\(.*\)%s\(.*\)', '\1\|\2', '')
    let commentstr = substitute(commentstr, '|$', '', '')
    let commentstr = substitute(commentstr, '\([\[\]\$\^\.\*|\\]\)',
                                \ '\\\1', 'g')
    let line = substitute(line, commentstr, '', 'g')
    let line = substitute(line, '===\+$', '', '')
    let line = substitute(line, '---\+$', '', '')
    " Replace initial whitespace with dashes indicating foldlevel
    " let line = substitute(line, '^\s*', v:folddashes . ' ', '')
    return '|' . v:folddashes .' '. line . '[ ' . foldedlinecount . ' ] '
endfunction " }}}
function! WordCount() " {{{
  let s:oldstat = v:statusmsg
  let position = getpos('.')
  exe ":silent normal g\<c-g>"
  let status = v:statusmsg
  let s:wordcount = ''

  if status != '--No lines in buffer--' && mode() !~? '[v]'
    let s:wordcount = str2nr(split(status)[11]) . 'w'
  endif

  let v:statusmsg = s:oldstat
  call setpos('.', position)

  return s:wordcount
endfunction " }}}
if executable('ranger') " {{{
  function! RangeChooser()
    let temp = tempname()
    " The option --choosefiles was added in ranger 1.5.1. Use the next line
    " with ranger 1.4.2 through 1.5.0 instead.
    " exec 'silent !ranger --choosefile=' . shellescape(temp)
    exec 'silent !ranger --choosefiles=' . shellescape(temp)
    if !filereadable(temp)
      redraw!
      " Nothing to read
      return
    endif
    let names = readfile(temp)
    if empty(names)
      redraw!
      " Nothing to open
      return
    endif
    " Edit the first item
    exec 'edit ' . fnameescape(names[0])
    " Add any remaining items to the arg/buffer list
    for name in names[1:]
      exec 'argadd ' . fnameescape(name)
    endfor
    redraw!
  endfunction
  command! -bar RangerChooser call RangeChooser()
  nnoremap <leader>f :<C-u>RangerChooser<CR>
endif " }}}
function! CharToEnd(char) " {{{
  let s:l = len(getline('.')) + 1
  if &textwidth > 0
    let s:e = &textwidth
  elseif exists("g:tw")
    let s:e = g:tw
  else
    let s:e = 78
  endif
  exe "normal! :s/\s*$/ /e"
  exe "normal! " . (s:e - s:l) . "A" . a:char
endfunction " }}}
function! ChTabBuf(motion) " {{{
    if tabpagenr('$') == 1
        " there is only 1 tab; switch buffers
        if a:motion < 0
            exe 'bprevious ' . abs(a:motion)
        else
            exe 'bnext ' . abs(a:motion)
        endif
    else
        if a:motion < 0
            exe 'tabprevious ' . abs(a:motion)
        else
            exe 'tabprevious ' . abs(tabpagenr('$') - a:motion)
        endif
    endif
endfunction " }}}
function! CloseBufWin() " {{{
    if len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) > 1
        bdelete
    else
        quit
    endif
endfunction " }}}
function! PopOpen(file) " {{{
    " Opens the file in a new tab, or current buffer if empty
    " TODO: add option for where to open file
    let s:fpath = fnameescape(a:file)
    if bufname("%") == ""
        execute "edit" s:fpath
    else
        execute "tabe" s:fpath
    endif
endfunction " }}}
function! ToggleBG(...) "{{{
  if a:0
    if a:1 ==? 'light'
      set bg=light
    elseif a:1 ==? 'dark'
      set bg=dark
    elseif a:1 =~? 'tog.*'
      if &bg == 'light'
          set background=dark
      else
          set background=light
      endif
    elseif a:1 =~? 'time' && exists("*strftime")
      if strftime("%H") > 6 && strftime("%H") < 18 " good enough sunrise & set
        set bg=light
      else
        set bg=dark
      endif
    endif
  else
    call ToggleBG('tog')
  endif
endfunction "}}}
function! FoldZipper(switch) " {{{
  function! s:foldzip_on()
    let s:foldc = &foldclose
    let s:foldo = &foldopen
    set foldclose=all
    set foldopen=all
    return 0
  endfunction
  function! s:foldzip_off()
    let &foldclose = get(s:, 'foldc', &foldclose)
    let &foldopen  = get(s:, 'foldo', &foldopen)
    return 1
  endfunction

  if a:switch ==? 'on'
    let b:foldzip_enabled = <SID>foldzip_on()
  elseif a:switch ==? 'off'
    let b:foldzip_enabled = <SID>foldzip_off()
  else
    let b:foldzip_enabled = get(b:, 'foldzip_enabled', 0)
                          \ ? <SID>foldzip_off()
                          \ : <SID>foldzip_on()
  endif
endfunction " }}}
function! ListPlus(switch) "{{{
    function! s:listplus_off()
        let s:cul  = &l:cursorline
        " let s:cc   = &l:colorcolumn
        let s:list = &l:list
        let s:rnu  = &l:relativenumber

        setlocal nocursorline
        " setlocal colorcolumn=
        setlocal nolist
        setlocal norelativenumber
        match none /\s\+$/

        return 0
    endfunction

    function! s:listplus_on()
        let &l:cul  = get(s:, 'cul',  &cursorline)
        let &l:cc   = get(s:, 'cc',   &colorcolumn)
        let &l:list = get(s:, 'list', &list)
        let &l:rnu  = get(s:, 'rnu',  &relativenumber)
        match Error /\s\+$/

        return 1
    endfunction

    if a:switch ==? 'on'
        let b:listplus_enabled = <SID>listplus_on()
    elseif a:switch ==? 'off'
        let b:listplus_enabled = <SID>listplus_off()
    elseif a:switch =~? 'tog'
        let b:listplus_enabled = get(b: 'listplus_enabled', 0)
                    \ ? <SID>listplus_off()
                    \ : <SID>listplus_on()
    endif
endfunction "}}}
function! RealScrollTo(direction) " {{{
    let s:scroff = &scrolloff
    set scrolloff=0

    if a:direction =~ 't'
        exe "normal! zt" . s:scroff . "j"
    elseif a:direction =~ 'b'
        exe "normal! zb" . s:scroff . "k"
    endif

    let &scrolloff = s:scroff
    unlet s:scroff
endfunction " }}}
function! s:NextTextObject(motion, dir) "{{{
  let c = nr2char(getchar())

  if c ==# "b"
    let c = "("
  elseif c ==# "B"
    let c = "{"
  elseif c ==# "r"
    let c = "["
  endif

  exe "normal! " . a:dir . c . "v" . a:motion . c
endfunction "}}}
function! LineMotion(dir) " {{{
  execute "normal! " . (v:count1 > 1 ? "m'" . v:count1 : "g") . a:dir
endfunction " }}}
function! UnIntelligent() "{{{
  " remove smartquotes, em-dashes, en-dashes, and other
  " 'fancy' typographical shit and replace them with pandoc-okay
  " stuff.  Because this is VIM goddammit, and I have
  " post-processors for that.
  let s:gdef_save  = &gdefault
  set nogdefault

  normal!  mz
  %sm/[‘’]/'/ge
  %sm/[“”]/"/ge
  %sm/—/---/ge
  %sm/–/--/ge
  normal! `z

  let &gdefault = s:gdef_save
endfunction "}}}
" }}} ========================================================================
" AUTOCOMMANDS {{{ =======================================================
augroup MakesSense " {{{
  au!
  au FocusLost   * silent! wall
  au BufEnter    * if &ft != 'help'
                 \ |  silent! lcd %:p:h
                 \ | endif
  au BufReadPost * normal `"
augroup END " }}}
augroup WindowCmds " {{{
  au!
  au VimEnter,WinEnter,BufWinEnter * call <SID>RefreshStatus()
  au VimEnter * call ToggleBG('time')

  au InsertEnter * call ListPlus('off')
  au InsertLeave * call ListPlus('on')

  " au BufWinLeave * set norelativenumber
  " au BufWinEnter * set relativenumber

  au BufWinLeave * silent! mkview
  au BufWinEnter * silent! loadview
augroup END " }}}
augroup FileTriggers " {{{
  au!
  " au BufEnter,BufRead xmonad.hs :call <SID>XMonadConfSettings()<CR>
augroup END
" function! s:XMonadConfSettings() " {{{
"   if exists('g:loaded_vimproc')
"   nnoremap <Leader>xx :w<CR>
"   :call vimproc#system("xmonad --recompile && xmonad --restart")<CR>
"  else
"   nnoremap <Leader>xx :w<CR>
"   :!xmonad --recompile && xmonad --restart<CR>
"   endif
" endfunction " }}}
" }}}
" FILETYPE {{{ -----------------------------------------------------------
" TODO: add :set define,include
augroup FT_help " {{{
  au!
  au FileType help setlocal nospell
  au BufWinEnter *.txt
        \ if &ft == 'help' && winwidth(0) >= 2 * g:tw
        \ |  wincmd L
        \ | endif
  au FileType help call ListPlus('off')
  au FileType help nnoremap <buffer> <CR> <C-]>
  au FileType help nnoremap <buffer> <BS> <C-t>
  " au FileType help nnoremap <buffer> o /'\S\{2,\}'<CR>
  " au FileType help nnoremap <buffer> O ?'\S\{2,\}'<CR>
  " au FileType help nnoremap <buffer> s /|\S\+\|<CR>l
  " au FileType help nnoremap <buffer> S ?|\S\+\|<CR>l
  " au FileType help nnoremap <buffer> t /\*\S\+\*<CR>l
  " au FileType help nnoremap <buffer> T ?\*\S\+\*<CR>l
  au FileType help nnoremap <buffer> q :q<CR>
augroup END " }}}
augroup FT_text " {{{
  au!
  au BufNewFile,BufRead,BufWrite *.txt,*.md
        \ if &ft != 'help' | setf pandoc | endif

  au FileType *wiki,markdown,pandoc setlocal spell
  au FileType *wiki,markdown,pandoc setlocal complete+=kspell
  au FileType *wiki,markdown,pandoc let b:texty = 1

  au CursorHold *.{txt,m*d*}
        \ if &modifiable
        \ |  update
        \ | endif
augroup END " }}}
augroup FT_netrw " {{{
  au!
  au FileType netrw nnoremap <buffer> <Esc> :bd!<CR>
augroup END " }}}
augroup FT_haskell " {{{
  au!
  au FileType haskell let &formatprg="stylish-haskell"
augroup END " }}}
augroup FT_vim " {{{
  au!
  au FileType vim setlocal keywordprg=
augroup END " }}}
" }}} ------------------------------------------------------------------------
" }}} ========================================================================
" PLATFORM {{{ ===========================================================
if has('win32') || has('win64') " {{{
  set runtimepath+=$HOME/.vim,$HOME/.vim/after
  set viminfo+=rA:,rB:
  augroup VimEnterWin
    au VimEnter * let &columns = 8 + g:tw + &fdc + &nu * &nuw
    au VimEnter * let &lines = 36
  augroup END
endif " }}}
" Create .vim directories if they don't exist {{{  ---------------------------
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
endif " }}} ------------------------------------------------------------------
" }}} ========================================================================
" PLUGINS {{{ ============================================================
" Automatic vim-plug installation {{{
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !mkdir -p ~/.vim/autoload
    silent !curl -fLo ~/.vim/autoload/plug.vim
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    au VimEnter * PlugInstall
endif " }}}
call plug#begin('~/.vim/plugged') " Vim-plug {{{
" Colorschemes {{{ -----------------------------------------------------------
" }}} ------------------------------------------------------------------------
" Writing {{{ ----------------------------------------------------------------
Plug 'junegunn/goyo.vim',
      \ { 'on': 'Goyo' }
  let g:goyo_width = g:tw + 1
  let g:goyo_margin_top = 2
  let g:goyo_margin_bottom = g:goyo_margin_top
  nnoremap <F11> :Goyo<CR>
Plug 'junegunn/limelight.vim',
      \ { 'on': 'Limelight' }
Plug 'tpope/vim-capslock'
Plug 'kana/vim-textobj-user'
Plug 'reedes/vim-textobj-sentence'
  augroup text_sentence
    au!
    au FileType pandoc,markdown call textobj#sentence#init()
  augroup END
Plug 'reedes/vim-textobj-quote'
  augroup text_quote
    au!
    au FileType pandoc,markdown call textobj#quote#init({'educate': 0})
  augroup END
  map <silent> <leader>'c <Plug>ReplaceWithCurly
  map <silent> <leader>'s <Plug>ReplaceWithStraight
" }}} ------------------------------------------------------------------------
" Coding {{{ -----------------------------------------------------------------
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
"Plug 'ervandew/supertab'
" }}} ------------------------------------------------------------------------
" Filesystem {{{ -------------------------------------------------------------
Plug 'ctrlpvim/ctrlp.vim'
  let g:ctrlp_map = 'gf'
  let g:ctrlp_use_caching = 1
  let g:ctrlp_clear_cache_on_exit = 0
  let g:ctrlp_cache_dir = $HOME . '/.vim/cache/ctrlp'
  let g:ctrlp_follow_symlinks = 1
  let g:ctrlp_max_depth = 100
  let g:ctrlp_max_files = 0
  let g:ctrlp_match_window = 'bottom,order:ttb'
  let g:ctrlp_lazy_update = 1
  let g:ctrlp_extensions = [ 'dir', 'line', 'mixed' ]
  let g:ctrlp_status_func = {
              \ 'main': 'CtrlPStatusLine',
              \ 'prog': 'CtrlPProgressLine'
              \ }
  let g:ctrlp_customignore = {
              \ 'dir': '\v[\/]\.(tmp|git|hg|svn)$',
              \ 'file': '\v\.(exe|so|dll|hi)$',
              \ }
  let g:ctrlp_reuse_window = 'startify'
  nnoremap gf :CtrlP<CR>
  nnoremap go :CtrlPMRU<CR>
  nnoremap gb :CtrlPBuffer<CR>
" if has('win64')
"   Plug 'Shougo/vimproc.vim'
"   let g:vimproc#dll_path = '~/.vim/vimproc_win64.dll'
" endif
" Plug 'Shougo/Unite.vim'
"   if executable('ag')
"     let g:unite_source_rec_async_command =
"           \ ['ag', '--follow', '--nocolor', '--nogroup',
"           \  '--hidden', '-g', '']
"     let g:unite_source_rec_find_args = []
"   endif
"   nnoremap <Leader>f :<C-u>Unite -start-insert file_rec<CR>
"   nnoremap <Leader>o :<C-u>Unite -start-insert file_mru<CR>
"   let g:unite_source_history_yank_enable = 1
"   nnoremap <Leader>y :<C-u>Unite history/yank<CR>
"   nnoremap go :<C-u>Unite -start-insert buffer file_mru file_rec/async<CR>
" Plug 'Shougo/neomru.vim'
" Plug 'kopischke/unite-spell-suggest'
" Plug 'sanford1/unite-unicode'
if has('python') && v:version >= 704
  Plug 'FelikZ/ctrlp-py-matcher'
endif
Plug 'dockyard/vim-easydir'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/gitignore'
Plug 'mhinz/vim-startify'
  let g:startify_session_dir = $HOME . '.vim/sessions'
  let g:startify_bookmarks = [ g:myvimrc,
                             \ ]
  let g:startify_custom_indices = [ 'r',
                                  \ '1', '2', '3', '4', '5',
                                  \ '6', '7', '8', '9', '0',
                                  \ 'f', 'g', 'h', 'd', 'a',
                                  \ 'l', 'w', 'y', 'u', 'z',
                                  \ 'x', 'c', 'n', 'm',    ]
  let g:pad = repeat(' ', (&columns / 2) - 6)
  let g:startify_custom_header = [
                         \ g:pad . '┌─────────┐',
                         \ g:pad . '│  BEGIN. │',
                         \ g:pad . '└─────────┘',
                         \ ]
  let g:startify_list_order = [
              \ ['█▒░ ❤ '], 'bookmarks',
              \ ['█▒░ … '], 'files',
              \ ['█▒░ ⌂ '], 'dir',
              \ ['█▒░ ☼ '], 'sessions',
              \ ]
  let g:startify_skiplist = [
              \ 'COMMIT_EDITMSG',
              \ '.git',
              \ 'plugged[\\/].*[\\/]doc',
              \ substitute($VIM, '\\', '[\\\\/]', 'g') . '.*'
              \ ]
" }}} ------------------------------------------------------------------------
" Extending Vim's behavior {{{ -----------------------------------------------
" Search & Replace
Plug 'nelstrom/vim-visual-star-search'
Plug 'tpope/vim-abolish'
  nnoremap gS :%S/
  xnoremap gS :S/
Plug 'haya14busa/incsearch.vim'
  let g:incsearch#auto_nohlsearch = 1
  let g:incsearch#consistent_n_direction = 1
  let g:incsearch#do_not_save_error_message_history = 1
  "let g:incsearch#magic = '\v'
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
" Aligning text
"Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'
  xnoremap \| :EasyAlign //<Left>
  vmap <Enter> <Plug>(EasyAlign)
  nmap ga <Plug>(EasyAlign)
Plug 'shinokada/dragvisuals.vim'
  vmap <expr> <LEFT>  DVB_Drag('left')
  vmap <expr> <RIGHT> DVB_Drag('right')
  vmap <expr> <DOWN>  DVB_Drag('down')
  vmap <expr> <UP>    DVB_Drag('up')
  vmap <expr> D       DVB_Duplicate()
  let g:DVB_TrimeWS = 1
" Extended operators
Plug 'AndrewRadev/splitjoin.vim'
  let g:splitjoin_split_mapping = 'gK'
  nnoremap <silent> J :<C-u>call <SID>try('SplitjoinJoin', 'mzJ`z')<CR>
  nnoremap <silent> K :<C-u>call <SID>try('SplitjoinSplit',
                                         \ "i\r\ed^kg_lD")<CR>
Plug 'tommcdo/vim-exchange'
Plug 'rhysd/clever-f.vim'
  let g:clever_f_smart_case = 1
  let g:clever_f_chars_match_any_signs = ';'
"Plug justinmk/vim-sneak'
" Text objects
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'tpope/vim-surround'
" Miscellaneous
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'nixon/vim-vmath'
  vmap <expr> ++ VMATH_YankAndAnalyse()
  nmap        ++ vip++
Plug 'ajh17/VimCompletesMe'
  " augroup VimCompletesWriting
  "   au!
  "   au FileType pandoc,markdown,text,*wiki
  "         \ let b:vcm_tab_complete = 'dict'
  " augroup END
" }}} ------------------------------------------------------------------------
" Filetypes {{{ --------------------------------------------------------------
" ------------- General
"Plug 'scrooloose/syntastic'
"  let g:syntastic_always_populate_loc_list = 1
"  let g:syntastic_auto_loc_list = 1
"  let g:syntastic_check_on_open = 1
"  let g:syntastic_check_on_wq = 0
Plug 'vim-scripts/matchit.zip'
Plug 'vim-scripts/SyntaxAttr.vim'
" ------------- HTML, CSS
Plug 'othree/html5.vim'
Plug 'gregsexton/MatchTag'
Plug 'amirh/HTML-AutoCloseTag'
Plug 'hail2u/vim-css3-syntax'
Plug 'gorodinskiy/vim-coloresque'
" ------------- Pandoc
Plug 'vim-pandoc/vim-pandoc',
      \ { 'for': [ 'pandoc', 'markdown' ] }
  let g:pandoc#modules#disabled = [
        \ 'menu',
        \ 'bibliographies',
        \ 'command',
        \ ] " TODO: see about Linux okay-ness
  let g:pandoc#command#custom_open = "PandocOpen" " function defined below
  let g:pandoc#command#use_message_buffers = 0
  let g:pandoc#filetypes#handled = [ 'markdown', 'rst', 'textile' ]
  let g:pandoc#folding#fdc = &fdc
  let g:pandoc#formatting#mode = "hA" " hard wrap, autoformat smart
  let g:pandoc#formatting#textwidth = g:tw
  let g:pandoc#spell#default_langs = ['en']
  let g:pandoc#toc#position = "left" " Table of contents
  let g:pandoc#toc#close_after_navigating = 0 " <CR> navs, <C-CR> navs + closes
Plug 'vim-pandoc/vim-pandoc-syntax'
  let g:pandoc#syntax#conceal#use = 0 " don't use conceal to hide any chars
  let g:pandoc#syntax#conceal#urls = 0
  nnoremap <Leader>pf :call pandoc#formatting#ToggleAutoformat()<CR>
                    \ :echo b:pandoc_autoformat_enabled
                    \ ? "Autoformat enabled"
                    \ : "Autoformat disabled"
                    \ <CR>
" Plug 'gbgar/pandoc-sections.vim',
"       \ { 'for': [ 'pandoc', 'markdown' ] }
Plug 'reedes/vim-litecorrect',
      \ { 'for': [ 'pandoc', 'markdown', 'text' ] }
" ------------- Haskell
Plug 'Twinside/vim-hoogle'
"Plug 'neovimhaskell/haskell-vim'
if has('python')
    Plug 'gilligan/vim-textobj-haskell'
endif
" ------------- Other
"Plug 'freitass/todo.txt-vim'
Plug 'LnL7/vim-nix'
Plug 'dogrover/vim-pentadactyl'
" }}} ------------------------------------------------------------------------
" System integration {{{ -----------------------------------------------------
if executable('git') " {{{
  Plug 'tpope/vim-fugitive'
    nnoremap <Leader>gs :w<CR>:Gstatus<CR>
  Plug 'tpope/vim-git'
  "Plug 'int3/vim-extradite'
  Plug 'airblade/vim-gitgutter'
  Plug 'esneider/YUNOcommit.vim'
    let g:YUNOcommit_after = 15
  set wildignore+=COMMIT_EDITMSG
endif " }}}
if executable('ag') " {{{
  Plug 'rking/ag.vim'
  let g:agprg = 'ag --column --smart-case'
  let g:aghighlight = 1
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g "" '
endif " }}}
if executable('ghc-mod') && has('unix') " {{{
  Plug 'Shougo/vimproc.vim',
        \ { 'do': 'make' }
  Plug 'eagletmt/ghcmod-vim'
  Plug 'eagletmt/neco-ghc'
endif " }}}
if has('unix') " {{{
  Plug 'tpope/vim-eunich'
endif " }}}
" Xolox {{{
Plug 'xolox/vim-misc'
Plug 'xolox/vim-shell'
  let g:shell_fullscreen_message = 0
  let g:shell_mappings_enabled   = 0
Plug 'xolox/vim-session'
  let g:session_autosave = 'no'
" }}}
if executable('wmctrl') " {{{
  Plug 'gioele/vim-autoswap'
endif " }}}
" }}} ------------------------------------------------------------------------
let g:plug_url_format = 'https://github.com/%s.git' " Plugins I've forked {{{
  Plug 'duckwork/gruvbox'
  Plug 'duckwork/vim2hs'
    let g:hpaste_author = 'duckwork'
    let g:haskell_conceal_wide = 0
    let g:haskell_conceal = 0
unlet g:plug_url_format " }}}
call plug#end() " }}}
" Functions for plugins {{{ --------------------------------------------------
function! CtrlPStatusLine(...) " {{{
    " arguments: focus, byfname, s:regexp, prv, item, nxt, marked
    "            a:1    a:2      a:3       a:4  a:5   a:6  a:7
    let regex = a:3 ? 'regex ' : ''
    let prv = ' '.a:4.' '
    let item = '%* ' . (a:5 == 'mru files' ? 'mru' : a:5) . ' %#CursorLine#'
    let nxt = '>'.a:6.' '
    let byfname = ' '.a:2.' '
    let dir = '%* ' . fnamemodify(getcwd(), ':~') . ' '

    return '%#CursorLineNr# π ' . item . nxt . ' %=%<' . dir
endfunction " }}}
function! CtrlPProgressLine(...) " {{{
    let len = '%#Function# '.a:1.' %*'
    let dir = ' %=%<%#LineNr# '.getcwd().' %*'
    return len . dir
endfunction " }}}
function! PandocOpen(file) " {{{
    if has('win32')
        return 'start '. a:file
    elseif executable('xdg-open')
        return 'xdg-open '. a:file
    else
        return a:file
    endif
endfunction " }}}
" Fallback functions {{{
" from noahfrederick.com/log/vim-and-progressive-enhancement/
function! s:try(cmd, default)
    if exists(':' . a:cmd) && !v:count
        let tick = b:changedtick
        exe a:cmd
        if tick == b:changedtick
            exe 'normal! ' . a:default
        endif
    else
        exe 'normal! ' . v:count . a:default
    endif
endfunction
" }}}
" }}} ------------------------------------------------------------------------
" }}} ========================================================================
colorscheme gruvbox
