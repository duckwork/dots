" __VARIABLES__ {{{ ==========================================================
let g:tw = 78 " fake textwidth
if filereadable(glob("~/dots/vimrc"))
let g:myvimrc = "~/dots/vimrc"
else
let g:myvimrc = $MYVIMRC
endif
let g:is_bash = 1
" }}} ========================================================================
" __SETTINGS__ {{{ ===========================================================
"  1 important {{{ -----------------------------------------------------------
set nocompatible " be iMproved
set cpoptions+=J " sentences are delimeted by 2 spaces
" }}} ------------------------------------------------------------------------
"  2 moving around, searching and patterns {{{ -------------------------------
" The following keys wrap movement around lines in the given modes:
set whichwrap=b,s " <BS> and <Space>
set ww      +=<,> " Arrow keys {N, V}
set ww      +=[,] " Arrow keys {I, R}
set nostartofline " Move to the start of line with movement
"set cdpath=,,
set wrapscan " Searches wrap around file ends
set incsearch " Show match for partly typed search
set magic " Backslashes quote more in regexes
set ignorecase " Ignore case when searching
set smartcase " ... unless the pattern has upper case
"  }}} -----------------------------------------------------------------------
"  3 tags {{{ ----------------------------------------------------------------
"  TODO
"  }}} -----------------------------------------------------------------------
"  4 displaying text {{{ -----------------------------------------------------
set scrolloff=8 " number of lines to show around cursor
set wrap " long lines wrap
set linebreak " wrap long lines to a character in 'breakat'
set breakindent " preserve indents in wrapped text
set breakindentopt=min:20 " broken text will be > 20 chars
"set briopt       +=shift:-2 " subsequent lines shift on breaks
set briopt       +=sbr " show the 'showbreak' before indent
let &showbreak = '└ '
set sidescroll=1 " minimal number of columns to scroll
set sidescrolloff=1 " number of columns to show around cursor
set display=lastline " show as much of the window's last line as possible
" Characters to fill empty space in the following lines:
let &fillchars = 'stl:^' " Focused statusline
let &fcs      .= ',stlnc: ' " Unfocused statusline
let &fcs      .= ',vert:|' " Vertical window separators
let &fcs      .= ',fold:-' " 'foldtext'
let &fcs      .= ',diff:-' " deleted lines of 'diff' option
set cmdheight=1 " number of lines to use for the command-line
set lazyredraw " don't redraw while executing macros
set list
" Characters to replace non-printing characters with:
let &listchars = 'tab:» ' " <Tab>s
let &lcs      .= ',trail:·' " Trailing whitespace
let &lcs      .= ',extends: ' " 'nowrap': line goes past right
let &lcs      .= ',precedes: ' " 'nowrap': line goes past left
let &lcs      .= ',eol: ' " end-of-line
let &lcs      .= ',nbsp:~' " Non-breaking spaces
set number " show line number for each line
"set relativenumber " show relative line numbers < AUCMD
set numberwidth=4 " minimum width of line number column
"  }}} -----------------------------------------------------------------------
"  5 syntax, highlighting and spelling {{{ -----------------------------------
set background=dark " background color brightness
set synmaxcol=300 " maximum column to syntax highlight to
set hlsearch " highlight all matches for last used search
set nocursorcolumn " highlight screen column of cursor
set cursorline " highlight the screen line of cursor
let &colorcolumn = g:tw + 1 " columns to highlight
set spelllang=en_us " list of accepted languages
set spellfile=~/.vim/spell/en_us.utf-8.add " file to store custom words
"  }}} -----------------------------------------------------------------------
"  6 multiple windows {{{ ----------------------------------------------------
set laststatus=2 " always show statusline
"set statusline= "__UpdateStatus()
set equalalways " resize windows to accomodate changes
set eadirection=both " which direction to 'equalalways'
set winheight=20 " minimal lines for current window
set winminheight=2 " minimum lines for any window
let &winwidth = g:tw + 1 " minimal columns for current window
set winminwidth=2 " minimum columns for any window
set hidden " don't unload a buffer when window closes
set switchbuf=useopen " how to switch to a buffer
set splitbelow " new windows open below current
set splitright " new windoes open to the right of current
"  }}} -----------------------------------------------------------------------
"  7 multiple tab pages {{{ --------------------------------------------------
"set showtabline=1
"set tabpagemax=10
"set tabline=
"set guitablable=
"set guitabtooltip=
"  }}} -----------------------------------------------------------------------
"  8 terminal {{{ ------------------------------------------------------------
"set term=$TERM
"set ttytype=$TERM
"set ttybuiltin
"set nottyfast
"set noweirdinvert
"set esckeys
"set scrolljump=1
"set ttyscroll=999
"set guicursor=...
set title
"set titlelen=85
"set titleold=Thanks\ for\ flying\ Vim
"set icon
"set iconstring=
"  }}} -----------------------------------------------------------------------
"  9 using the mouse {{{ -----------------------------------------------------
if has('mouse')
  set mouse=a " enable mouse
  "set nomousefocus
  "set mousehide
  set mousemodel=popup " what the right mouse button is for
  "set mousetime=500
  "set ttymouse=
  "set mouseshape=...
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
  "set guifontwide=
  set guioptions=a " autocopy Vim's selection
  set go       +=c " use console instead of popups for choices
  set go       +=g " grey out inactive menu items
  set go       +=i " use Vim icon
  set go       +=t " include tearoff menus
  set browsedir=buffer " where to open the file browser
  set winaltkeys=no " how to map the <ALT> keys to the menu
  set linespace=1
endif
" }}} ------------------------------------------------------------------------
" 11 printing {{{ ------------------------------------------------------------
"set printoptions=
"set printdevice=
"set printexpr=...
"set printfont=courier
"set printheader=%<%f%h%m%=Page\ %N
"set printencoding=
"set printmbcharset=
"set printmbfont=
" }}} ------------------------------------------------------------------------
" 12 messages and info {{{ ---------------------------------------------------
set shortmess=a  " shm=filmnrwx
set shm     +=oO " overwrite file messages
set shm     +=tT " truncate messages in {C}
set shm     +=I  " no intro message
set showcmd " show partial commands as they're typed
set showmode " show current mode in statusline
set ruler " show ruler position in statusline
set report=5 " threshold for reporting number of changed lines
set verbose=0
set verbosefile=~/.vim/log.txt
set confirm " ask to save before quitting
set noerrorbells " ring the bell for error messages
set visualbell " use a visual bell instead of sound
set helplang=en
" }}} ------------------------------------------------------------------------
" 13 selecting text {{{ ------------------------------------------------------
set selection=exclusive " how selecting text behaves
if has('unnamedplus')
  set clipboard=autoselect,unnamedplus,exclude:cons\\\|linux
else
  set clipboard=autoselect,unnamed,exclude:cons\\\|linux
endif
set keymodel=startsel,stopsel " select with <Home>,<End>,<PgUp>,<PgDn>
" }}} ------------------------------------------------------------------------
" 14 editing text {{{ --------------------------------------------------------
set undolevels=10000 " max number of undoable changes
set undoreload=10000 " max lines to save for undo on reload
set textwidth=0 " where to break a line (0 -> don't)
set backspace=indent " allow backspacing over autoindent
set bs      +=eol " allow backspacing over line breaks
set bs      +=start " allow backspacing over start of insert
set formatoptions-=ro " disable autocomments in {I}
set tildeop " make <~> command behave like an operator
"set matchpairs=(:),{:},[:]
set nrformats=alpha " inc/dec alphabetical chars w/<C-a>,<C-x>
set nf      +=hex   " ''      hexadecimal nums   ''
" }}} ------------------------------------------------------------------------
" 15 tabs and indenting {{{ --------------------------------------------------
set shiftwidth=2 " number of spaces to autoindent
set smarttab " a <Tab> in an indent inserts 'shiftwidth' spaces
set softtabstop=2 " number of spaces to a <Tab>
set shiftround " round to 'shiftwidth' for <<,>>
set expandtab " expand <Tab> to spaces in {I}
set autoindent " automatically set the indent of a new line
set smartindent " clever autoindenting
" }}} ------------------------------------------------------------------------
" 16 folding {{{ -------------------------------------------------------------
set foldenable
"set foldlevel=0
"set foldlevelstart=-1
set foldcolumn=1 " width of gutter column to indicate folds
set foldtext=FoldLine()
"set foldclose=all
"set foldopen=block,hor,mark,percent,quickfix,search,tag,undo
"set foldminlines=1
"set foldmethod=...
"set foldexpr=0
"set foldignore=#
"set foldmarker={{{,}}}
"set foldnestmax=20
" }}} ------------------------------------------------------------------------
" 17 diff mode {{{ -----------------------------------------------------------
"set nodiff
"set diffopt=filler
"set diffexpr=
"set patchexpr=
" }}} ------------------------------------------------------------------------
" 18 mapping {{{ -------------------------------------------------------------
"set maxmapdepth=1000
"set remap
"set timeout
"set ttimeout
set timeoutlen=1000
"set ttimeoutlen=-1
" }}} ------------------------------------------------------------------------
" 19 reading and writing files {{{ -------------------------------------------
set fileformat=unix " end-of-line format
set fileformats=unix,dos " list of eol formats to look for
set backup
set backupcopy=yes " whether to backup a copy or rename existing
set backupdir=$HOME/.vim/backup/ " directory to store backups
set autowriteall
set autoread " automatically read a file when modified outside Vim
" }}} ------------------------------------------------------------------------
" 20 the swap file {{{ -------------------------------------------------------
set directory=$HOME/.vim/swap/ " list of dirs for swap files
"set swapfile
" }}} ------------------------------------------------------------------------
" 21 command line editing {{{ ------------------------------------------------
set history=1000 " how many command lines are remembered
set wildmode=full
set wildignore+=*/.git/* " ignore git files in wildmenu
set wig       +=*/.hg/*  " ''     hg files ''
set wig       +=*/.svn/* " ''    svn files ''
"set nofileignorecase
set wildignorecase " ignore case when completing filenames
set wildmenu " show a list of matches w/ cmd completion
set undofile
set undodir=$HOME/.vim/undoes/
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
set encoding=utf-8 " character encoding used
set fileencoding=utf-8 " character encoding for file
"set fileencodings=...
"set termencoding=utf-8
"set charconvert=
"set nodelcombine
"set maxcombine=2
"set ambiwidth=single
" }}} ------------------------------------------------------------------------
" 26 various {{{ -------------------------------------------------------------
set virtualedit=block " allow positioning cursor anywhere in {^V}
set exrc " enable reading .vimrc in current directory
set secure " ..but don't let them do anything dangerous
set gdefault " use the 'g' flag by default in :s
set viewoptions=folds " save folds in view
set vop       +=cursor " save cursor position
set vop       +=slash " save backslashes as forward slashes
set vop       +=unix " save views with Unix eol format
set viewdir=$HOME/.vim/views/
set viminfo='100 " Save marks for 100 files max
set vi    ^=! " Save global all-caps variables
set vi    +=<50 " Save 50 lines of registers
set vi    +=s10 " Save only first 10 Kb of each register
set vi    +=h " Disable 'hlsearch' on saved files
" }}} ------------------------------------------------------------------------
" }}} ========================================================================
" __KEYMAPS__ {{{ ============================================================
" Leaders
let mapleader = ","
let maplocalleader = ","

" Changing modes
nnoremap ; :
inoremap jj <Esc>
inoremap kk <Esc>

" Basic movement
nnoremap <expr> j v:count > 0 ? 'j' : 'gj'
nnoremap <expr> k v:count > 0 ? 'k' : 'gk'
nnoremap H ^
nnoremap L $
nnoremap gH :call RealScrollTo('top')<CR>
nnoremap gM M
nnoremap gL :call RealScrollTo('bot')<CR>

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
nnoremap <silent> <leader>rs mz:%s\+$//<CR>:let @/=''<CR>`z
nnoremap <silent> <leader>rb mz:g/^$/d<CR>:let @/=''<CR>`z

" File formatting
nnoremap <Leader>gq mzgggqG`z
nnoremap <Leader>gu :update<CR>:e ++ff=dos<CR>:setlocal ff=unix<CR>:w<CR>

nnoremap <Leader>= :call CharToEnd("=")<CR>
nnoremap <Leader>- :call CharToEnd("-")<CR>

" Toggle settings
nnoremap <silent> <leader>bg :call ToggleBG()<CR>

" Clipboard
if has('unnamedplus')
  nnoremap <silent> <Leader>p :set paste<CR>"+p:set nopaste<CR>
  nnoremap <silent> <Leader>P :set paste<CR>"+P:set nopaste<CR>
else
  nnoremap <silent> <Leader>p :set paste<CR>"*p:set nopaste<CR>
  nnoremap <silent> <Leader>P :set paste<CR>"*P:set nopaste<CR>
endif

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
"nnoremap <F11>
nnoremap <F12> :call CloseBufWin()<CR>
" }}} ========================================================================
" __FUNCTIONS__ {{{ ==========================================================
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

function! FoldLine() " {{{
    let line = getline(v:foldstart)
    let foldedlinecount = printf('%3d', v:foldend - v:foldstart)
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
    " Replace initial whitespace with dashes indicating foldlevel
    let line = substitute(line, '^\s*', v:folddashes . ' ', '')
    return '> ' . v:foldlevel . ': ' . foldedlinecount . ' ' . line . ' <'
endfunction " }}}
function! StatusLine(winnr) " {{{
  let status = ''

  let buffer = winbufnr(a:winnr)
  let fname = bufname(buffer)
  let ftype = getbufvar(buffer, '&filetype')

  let isactive = winnr() == a:winnr
  let ismodified = getbufvar(buffer, '&modified')
  let isreadonly = getbufvar(buffer, '&readonly')
  let ishelp = ftype == 'help'

  " Left side {{{ ------------------------------------------------------------
  " Ruler {{{
  if isactive
    if &number
      let status .= '>%3v '
    else
      let status .= '>%3l:%v '
    endif
  else
    let status .= ' [%n] '
  endif " }}}
  " Scroll {{{
  if !isactive
    let status .= '%#CursorLine#'
    let status .= ' %-3pp%%'
  else
    let status .= '| %-3pp%%'
    if exists("b:texty") " set by FT_text augroup
      let status .= ' | %{WordCount()}'
    endif
  endif " }}}
  " }}} ----------------------------------------------------------------------
  let status .= '%#CursorLine#%=' " Gutter -----------------------------------
  " Right side {{{ -----------------------------------------------------------
  " File status indicators {{{
  if isactive
    if ! isreadonly
      if ismodified
        let status .= '%#DiffAdd'
        let status = ' + '
      endif
    else
      if ishelp
        let status .= '%#DiffDelete'
        let status .= ' ? '
      else
        let status .= ' ! '
      endif
    endif
    if &paste
      let status .= '%#DiffChange'
      let status .= ' P '
    endif
  else
    if ! isreadonly
      if ismodified
        let status = '[+]'
      endif
    else
      if ishelp
        let status .= '[?]'
      else
        let status .= '[!]'
      endif
    endif
  endif " }}}
  " Filename {{{
  let fstat = ''
  let fsep = '::'
  if fname == ''
    let fstat .= '__'
    if len(ftype) > 0
      let fstat .= fsep . ftype
    endif
  else
    let fstat .= '%f'
    if len(ftype) > 0
      let fstat .= fsep . ftype
    endif
  endif

  if isactive
    let status .= '%#CursorLine#%<' . getcwd()
    if has('win32')
      let status .= '\'
    else
      let status .= '/'
    endif
    let status .= '%#StatusLine#'
    let status .= ' ' . fstat . ' '
  else
    let status .= '%< ' . fstat . ' '
  endif
  " }}}
  " }}} ----------------------------------------------------------------------
  return status
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

function! ToggleBG() "{{{
    if &bg == 'light'
        set background=dark
    else
        set background=light
    endif
endfunction "}}}
function! ListPlus(switch) "{{{
    function! s:listplus_off()
        let s:cul  = &l:cursorline
        let s:cc   = &l:colorcolumn
        let s:list = &l:list
        let s:rnu  = &l:relativenumber

        setlocal nocursorline
        setlocal colorcolumn=
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
" }}} ========================================================================
" __AUTOCOMMANDS__ {{{ =======================================================
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

  au InsertEnter * call ListPlus('off')
  au InsertLeave * call ListPlus('on')

  au BufWinLeave * set norelativenumber
  au BufWinEnter * set relativenumber

  au BufWinLeave * silent! mkview
  au BufWinEnter * silent! loadview
augroup END " }}}
augroup FileTriggers " {{{
  au!
  au BufEnter,BufRead xmonad.hs
        \ if exists('g:loaded_vimproc')
        \ |  nnoremap <Leader>xx :w<CR>
        \ |  :call vimproc#system("xmonad --recompile && xmonad --restart")<CR>
        \ | else
        \ |  nnoremap <Leader>xx :w<CR>
        \ |  :!xmonad --recompile && xmonad --restart<CR>
        \ | endif
" }}}
" __FILETYPE__ {{{ -----------------------------------------------------------
" TODO: add :set define,include
augroup FT_help " {{{
  au!
  au FileType help setlocal nospell
  au BufWinEnter *.txt
        \ if &ft == 'help' && winwidth(0) >= 2 * g:tw
        \ |  wincmd L
        \ | endif
  au FileType help nnoremap <buffer> <CR> <C-]>
  au FileType help nnoremap <buffer> <BS> <C-t>
  au FileType help call ListPlus('off')
augroup END " }}}
augroup FT_text " {{{
  au!
  au BufNewFile,BufRead,BufWrite *.txt,*.md
        \ setf pandoc

  au FileType *wiki,markdown,pandoc setlocal spell
  au FileType *wiki,markdown,pandoc setlocal complete+=kspell
  au FileType *wiki,markdown,pandoc let b:texty = 1

  au CursorHold *.{txt,m*d*}
        \ if &modifiable && &modified
        \ |  write
        \ | endif
augroup END " }}}
augroup FT_netrw " {{{
  au!
  au FileType netrw nnoremap <buffer> <Esc> :q!<CR>
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
" __PLATFORM__ {{{ ===========================================================
if has('win32') " {{{
  set runtimepath+=$HOME\\.vim
  set viminfo+=rA:,rB:
endif " }}}
" Create .vim directories if they don't exist {{{  ---------------------------
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
endif " }}} ------------------------------------------------------------------
" }}} ========================================================================
" __PLUGINS__ {{{ ============================================================
" Automatic vim-plug installation {{{
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !mkdir -p ~/.vim/autoload
    silent !curl -fLo ~/.vim/autoload/plug.vim
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    au VimEnter * PlugInstall
endif " }}}
call plug#begin('~/.vim/plugged')
" Colorschemes {{{ -----------------------------------------------------------
Plug 'duckwork/gruvbox'
" }}} ------------------------------------------------------------------------
" Writing {{{ ----------------------------------------------------------------
Plug 'junegunn/goyo.vim',
      \ { 'on': 'Goyo' }
  let g:goyo_width = g:tw + 1
  let g:goyo_margin_top = 2
  let g:goyo_margin_bottom = g:goyo_margin_top
  nnoremap <S-F11> :Goyo<CR>
Plug 'junegunn/limelight.vim',
      \ { 'on': 'Limelight' }
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
              \ 'dir': '\/var\/tmp$',
              \ }
  nnoremap gf :CtrlP<CR>
  nnoremap go :CtrlPMRU<CR>
  nnoremap gb :CtrlPBuffer<CR>
if has('python') && v:version >= 704
  Plug 'FelikZ/ctrlp-py-matcher'
endif
Plug 'dockyard/vim-easydir'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/gitignore'
" }}} ------------------------------------------------------------------------
" Extending Vim's behavior {{{ -----------------------------------------------
" Search & Replace
Plug 'nelstrom/vim-visual-star-search'
Plug 'tpope/vim-abolish'
  nnoremap gS :%S/
  xnoremap gS :S/
" Aligning text
"Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'
  xnoremap \| :EasyAlign //<Left>
  vmap <Enter> <Plug>(EasyAlign)
  nmap ga <Plug>(EasyAlign)
" Extended operators
Plug 'AndrewRadev/splitjoin.vim'
  let g:splitjoin_split_mapping = 'gK'
  nnoremap <silent> J :<C-u>call <SID>try('SplitjoinJoin', 'J')<CR>
  nnoremap <silent> K :<C-u>call <SID>try('SplitjoinSplit',
                                         \ "i\r\ed^kg_lD")<CR>
Plug 'tommcdo/vim-exchange'
Plug 'rhysd/clever-f.vim'
  let g:clever_f_smart_case = 1
  let g:clever_f_chars_match_any_signs = ';'
" Text objects
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'tpope/vim-surround'
" Miscellaneous
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
" }}} ------------------------------------------------------------------------
" Filetypes {{{ --------------------------------------------------------------
"Plug 'scrooloose/syntastic'
"  let g:syntastic_always_populate_loc_list = 1
"  let g:syntastic_auto_loc_list = 1
"  let g:syntastic_check_on_open = 1
"  let g:syntastic_check_on_wq = 0
Plug 'vim-scripts/matchit.zip'
Plug 'vim-scripts/SyntaxAttr.vim'

Plug 'gregsexton/MatchTag',
      \ { 'for': [ 'html', 'xml', ] }

Plug 'vim-pandoc/vim-pandoc',
      \ { 'for': [ 'pandoc', 'markdown' ] }
  let g:pandoc#modules#disabled = [ 'menu' ] " Get rid of Pandoc menu
  let g:pandoc#command#custom_open = "PandocOpen" " function defined below
  let g:pandoc#command#use_message_buffers = 0
  let g:pandoc#filetypes#handled = [ 'markdown', 'rst', 'textile' ]
  let g:pandoc#folding#fdc = &fdc
  " let g:pandoc#formatting#mode = 'h' " hard wrap, autoformat smart
  let g:pandoc#formatting#textwidth = g:tw
  let g:pandoc#keyboard#sections#header_style = 's' " enable setext for h1,2
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
Plug 'gbgar/pandoc-sections.vim',
      \ { 'for': [ 'pandoc', 'markdown' ] }
Plug 'reedes/vim-litecorrect',
      \ { 'for': [ 'pandoc', 'markdown', 'text' ] }

Plug 'duckwork/vim2hs'
  let g:hpaste_author = 'duckwork'
  let g:haskell_conceal_wide = 0
  let g:haskell_conceal = 0
Plug 'Twinside/vim-hoogle'
"Plug 'neovimhaskell/haskell-vim'

Plug 'hail2u/vim-css3-syntax'
Plug 'othree/html5.vim'
Plug 'dogrover/vim-pentadactyl'

"Plug 'freitass/todo.txt-vim'
Plug 'LnL7/vim-nix'
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
" }}} ------------------------------------------------------------------------
call plug#end()
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
