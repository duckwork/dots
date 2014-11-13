<<<<<<< HEAD
" ______________________ VIMRC -- CASE DUCKWORTH _________________________
" vim:foldlevel=0:textwidth=0:nowrap:nolinebreak
set nocompatible
let g:mytw = 78
" Part 0:   Vundle {{{
filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()
=======
" ViMrc ReVised
" Case Duckworth (mahatman2)
" vim:foldenable:foldlevel=0:tw=0:nowrap:nolinebreak
" TODO: Lightline config

set nocompatible " be iMproved

" SETTINGS {{{
syntax on                         " syntax highlighting

set number                        " show line numbers in gutter
set relativenumber                " show line nums relative to current line

set autoread                      " reload on a change, automagically
set lazyredraw                    " don't redraw macros til done
set hidden                        " Don't close unused buffers

set noerrorbells                  " don't beep on errors
set visualbell                    " flash instead of beeping

set splitbelow                    " new splits appear below current window
set splitright                    " new splits appear to right of current

set formatoptions-=ro             " disable autocomments in Insert mode
set backspace=indent,eol,start    " backspace across these things

" Which keys allow movement between lines
set whichwrap=b,s                 " <BS> and <Space>
set ww      +=<,>                 " Arrow keys (N, V)
set ww      +=[,]                 " Arrow keys (I, R)

set encoding=utf-8                " encoding = utf-8.
set fileencoding=utf-8            " because year = 2014.
set fileformat=unix               " Unix file ending default
set fileformats=unix,dos          " but recognize DOS line endings

set spelllang=en                  " I write in English

set history=1000                  " set history length

" What's saved between sessions in a viminfo file
set viminfo='100                  " Save marks for 100 files max
set vi    ^=!                     " Save global all-caps variables
set vi    +=<50                   " Save 50 lines of registers
set vi    +=s10                   " Save only first 10 Kb of each register
set vi    +=h                     " Disable 'hlsearch' on saved files

set laststatus=2                  " Always show statusline
" set statusline=%!StatusLine()     "   [see function below]
" set showtabline=2                 " Always show tabline
" set tabline=%!TabLine()           "   [see function below]
set wildmenu                      " Enhanced command-line completion
set ruler                         " Show where the cursor is in the file
set showcmd                       " Show command as it's being typed

set foldenable                    " Enable folding
set foldmethod=marker             " {{{ }}} mark folds
set foldlevel=2                   " Start open to second level
set foldcolumn=0                  " Fold columns in gutter
set foldtext=FoldLine()           " Define what folded folds look like

set scrolloff=8                   " Keep lines between cursor and edges
set sidescrolloff=2               " Keep columns between cursor and edges
set sidescroll=1                  " Scroll one column at a time sideways

let g:tw = 78                     " Set textwidth without really setting tw
" let &textwidth = g:tw             " Set textwidth
set wrap                          " Soft wrap lines to window
set linebreak                     " Wrap at words (def by 'breakat')
set breakindent                   " soft-wrapped lines indent to prev line
set listchars=tab:»»,trail:·,nbsp:~ " make it easy to see these

" Make too-long lines obvious, but only on those lines
call matchadd('ColorColumn', '\%'.g:tw.'v', 100)

set expandtab                     " use spaces instead of tabs
set autoindent                    " copy indent when inserting a new line
set smartindent                   " autoindent based on conditions
set shiftwidth=4                  " number of spaces for (auto)indent
set softtabstop=4                 " number of spaces a <Tab> equals
set shiftround                    " indent to nearest tabstop

set incsearch                     " find the next match as we type
set hlsearch                      " highlight search matches
set ignorecase                    " ignore case as we search
set smartcase                     " ... unless a capital appears

set gdefault                      " default to global (line) substitutions
set magic                         " use better regexp
" This is a test for a thing, yeah?
set directory=$HOME/.vim/swap//   " directory for swap files
set backupdir=$HOME/.vim/backup// " directory for backups
set backupcopy=yes                " copy the original and overwrite
set backup                        " Keep backup files, just in case

set undolevels=10000
"}}}
" KEYMAPS {{{
>>>>>>> lightline

" Easier mode changing
noremap ; :
inoremap jj <Esc>
inoremap kk <Esc>
" Better leader
let mapleader = ','

" Better movement
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
nnoremap H ^
nnoremap L $

" Better folding
nnoremap <Space> za
vnoremap <Space> za
nnoremap <S-Space> zA
vnoremap <S-Space> zA
nnoremap <C-Space> zM
nnoremap <C-S-Space> zR

" More consistent capital operators (C, D, Y)
nnoremap Y y$
" Make X opposite of D (delete to beginning of line)
nnoremap X d0

" <BS> backspaces in normal mode
nnoremap <BS> X
" Make K opposite of J (join).
nnoremap K i<CR><Esc>
" Make <F1> help better.
nnoremap <F1> K

" Keep selection after indenting
vnoremap <leader>> >gv
vnoremap <leader>< <gv

" Easier searching
nnoremap gs :%s/
xnoremap gs :s/

" Window management
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
nnoremap <S-UP>    <C-w>K
nnoremap <S-DOWN>  <C-w>J
nnoremap <S-LEFT>  <C-w>H
nnoremap <S-RIGHT> <C-w>L
nnoremap <C-UP>    <C-w>+
nnoremap <C-DOWN>  <C-w>-
nnoremap <C-LEFT>  <C-w>>
nnoremap <C-RIGHT> <C-w><

<<<<<<< HEAD
" PLUGINS THAT REQUIRE ...
if executable('git')
    Plugin 'airblade/vim-gitgutter' " Git stuff in signs column
    Plugin 'tpope/vim-fugitive'     " Git integration
endif
if executable('ag')
    Plugin 'rking/ag.vim'              " Ag implementation
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g "" '
endif
if has('python')
    Plugin 'vim-scripts/gundo'     " Visualize Vim's undo tree
endif

call vundle#end()                      "req'd
filetype plugin indent on              "req'd
"}}}
" Part I:   Plugin Config {{{
" --- Ctrl-P options {{{
let g:ctrlp_max_depth           = 100 " max depth of search
let g:ctrlp_max_files           = 0   " no limit to how many files
let g:ctrlp_use_caching         = 1   " enable caching
let g:ctrlp_clear_cache_on_exit = 0   " enable cross-session caching
let g:ctrlp_cache_dir           = $HOME . '/.cache/ctrlp'
let g:ctrlp_lazy_update         = 1   " Update only after done typing
let g:ctrlp_match_window        = 'bottom,order:ttb'
" --- }}}
" --- Airline options {{{
" --- --- Section definition {{{
let g:airline_section_a  = '%2c'
let g:airline_section_b  = "%<%f%{&modified ? ' +' : ''}"

let g:airline_section_c  = '%#__accent_red#%{airline#util#wrap(airline#parts#readonly(),0)}%#__restore__#'

" g:airline_section_y "{{{
let g:ywc = [
            \ '',
            \ '%{Count("words")}w',
            \ '%{Count("bytes")}c',
            \ '%{Count("thisw")}/%{Count("words")}w',
            \ ]
let g:ywci = len(g:ywc)
let g:airline_section_y  = g:ywc[0]
nnoremap <silent> <F2> :let g:ywci += 1<CR>
            \ :let g:airline_section_y = g:ywc[g:ywci % len(g:ywc)]<CR>
            \ :AirlineRefresh<CR>
" "}}}

 let g:airline_section_z  = '%P'
" let g:airline_section_z .= '_%02l'            " _ll
" let g:airline_section_z .= '|%02c'            " |cc
let g:airline_section_z .= '%{airline#util#append(airline#parts#paste(),0)}%{airline#util#append("",0)}%{airline#util#append(airline#parts#iminsert(),0)} '

let g:airline#extensions#default#section_truncate_width = {
    \ 'b': 40,
    \ 'x': 60,
    \ 'y': 68,
    \ 'z': 45,
    \ }
"}}}
" --- --- Font & symbol options {{{
let g:airline_powerline_fonts = 0
let g:airline_left_sep        = ''
let g:airline_right_sep       = ''

let g:airline_mode_map = {
    \ '__' : '--',
    \ 'n'  : 'Nr',
    \ 'i'  : 'In',
    \ 'R'  : 'Re',
    \ 'c'  : 'Cm',
    \ 'v'  : 'Vi',
    \ 'V'  : 'V_',
    \ '' : 'V[',
    \ 's'  : 'Se',
    \ 'S'  : 'S_',
    \ '' : 'S[',
    \ }
=======
" Edit and source vimrc easily
nnoremap <leader>ev :edit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Change current directory to filepath
nnoremap <leader>cd :cd %:p:h<CR>
" Explore the current directory
nnoremap <silent> \ :Explore<CR>

" Remove search highlight
nnoremap <leader>/ :nohlsearch<CR>
" Remove end-of-line whitespace
nnoremap <silent> <leader>rs mz:%s/\s\+$//<CR>:let @/=''<CR>`z
" Remove blank lines
nnoremap <silent> <leader>rb mz:g/^$/d<CR>:let @/=''<CR>`z

" Linux only: because `sudo vim` is easy to forget
cmap w!! %!sudo tee > /dev/null %
>>>>>>> lightline
"}}}
" AUTOCOMMANDS {{{
autocmd BufEnter * silent! lcd %:p:h
autocmd ColorScheme * call UpdateCursorLineNumber()

augroup TextEditing
    au!
    au BufNewFile,BufRead *.md set ft=markdown spell
    au FileTYpe vimwiki,markdown,text setlocal spell
augroup END

" augroup ReloadVimrc
"     au!
"     au BufWritePost $MYVIMRC source $MYVIMRC
" augroup END

augroup CursorLine
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

augroup TrailingSpaces
    au!
    au InsertEnter * match none '\s\+$'
    au InsertEnter * set nolist
    au InsertLeave * match Error '\s\+$'
    au InsertLeave * set list
augroup END

augroup ft_Help
    au!
    au FileType help setlocal nospell nocursorline
    au BufWinEnter *.txt
                \ if &ft == 'help' && winwidth(0) >=2 * g:tw |
                \     wincmd L |
                \ endif
augroup END

augroup DoStatusLine
    au!
    au VimEnter,WinEnter,BufWinEnter * call <SID>RefreshStatus()
augroup END
"}}}
" CAN I HAS(?) {{{
if has('win32') " {{{
    let &runtimepath .= ',$HOME\.vim'

    let &clipboard = has('unnamedplus') ? 'unnamedplus' : 'unnamed'

<<<<<<< HEAD
let g:goyo_width = g:mytw
let g:goyo_margin_top = 2
let g:goyo_margin_bottom = 2
=======
    set viminfo+=rA:,rB:

    let g:ctrlp_cmd = 'CtrlP D:\Dropbox'
endif " }}}
if has('mouse') " {{{
    set mouse=a
endif " }}}
if has('gui_running') " {{{
    " Remove all the GUI cruft
    set guioptions-=m " remove menu bar
    set go        -=T " remove toolbar
    set go        -=r " remove right-hand scrollbar
    set go        -=L " remove left-hand scrollbar
    set go        -=e " remove GUI tablines

    " Set colors to 256
    set t_Co=256

    " Change font based on system
    if has('gui_gtk2')
        set guifont=Inconsolata
    elseif has('x11')
        "set guifont=*-lucidatypewriter-medium-r-normal-*-*-180-*-*-m-*-*
    elseif has('gui_win32')
        set guifont=Consolas:h11:cANSI
    endif
>>>>>>> lightline

    " Start at this size
    augroup CustomSizeVim
        au!
        au VimEnter * let &columns = g:tw + &fdc + &nu * &nuw
        au VimEnter * set lines=36
    augroup END

endif " }}}
if has('persistent_undo') "{{{
    set undodir=$HOME/.vim/undoes//   " directory for UNDO TREE
    set undofile
    set undoreload=10000
endif "}}}
" Create .vim/* directories if they don't exist {{{
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), 'p')
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), 'p')
endif
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), 'p')
endif
" }}}
<<<<<<< HEAD
" Part II:  Custom functions {{{
function! Count(thing) " {{{ Count(words|bytes|thisw|thisb)
    let s:old_status = v:statusmsg
    let position = getpos(".")
    exe ":silent normal g\<c-g>"
    let stat = v:statusmsg
    let s:word_count = 0
    if stat != '--No lines in buffer--'
        if mode() ==? 'v'
            " TODO: with selections, show line & column count too
            " Selected {n} of {m} lines; {n} of {m} words; {n} of {m} bytes
            let s:words  = split(stat, '; ')[1]
            let s:bytes  = split(stat, '; ')[2]
            let s:things = {
                        \ 'words': str2nr(split(s:words)[2]),
                        \ 'thisw': str2nr(split(s:words)[0]),
                        \ 'bytes': str2nr(split(s:bytes)[2]),
                        \ 'thisb': str2nr(split(s:bytes)[0]),
                        \ }
        elseif mode() == ''
            " Selected {n} Cols; {n} of {m} lines; {n} of {m} words;
            " \ {n} of {m} bytes
            let s:words  = split(stat, '; ')[2]
            let s:bytes  = split(stat, '; ')[3]
            let s:things = {
                        \ 'words': str2nr(split(s:words)[2]),
                        \ 'thisw': str2nr(split(s:words)[0]),
                        \ 'bytes': str2nr(split(s:bytes)[2]),
                        \ 'thisb': str2nr(split(s:bytes)[0]),
                        \ }
=======
"}}}
" FUNCTIONS {{{
" --- Tools
function! Rulerer() "{{{ A better ruler?
        let s:oldstat = v:statusmsg
        let position  = getpos('.')
        exe ":silent normal g\<c-g>"
        let status    = split(v:statusmsg, '; ')
        let curmode   = mode()
        let r  = {}

        if status != ['--No lines in buffer--'] "{{{
            if curmode ==? 'v' " Visual or V-line; v:statusmsg =
                "  Selected {n} of {m} Lines; {n} of {m} Words;
                " \ {n} of {m} Bytes
                let r.lines = {
                            \ 'cur': str2float(split(status[0])[1]),
                            \ 'tot': str2float(split(status[0])[3])
                            \ }
                let r.words = {
                            \ 'cur': str2nr(split(status[1])[0]),
                            \ 'tot': str2nr(split(status[1])[2])
                            \ }
                let r.chars = {
                            \ 'cur': str2nr(split(status[2])[0]),
                            \ 'tot': str2nr(split(status[2])[2])
                            \ }
                let r.stmode = 'v'
            elseif curmode == '' " Visual block; v:statusmsg =
                " Selected {n} Cols; {n} of {m} Lines; {n} of {m} Words;
                " \ {n} of {m} Bytes
                let r.cols = { 'cur': str2nr(split(status[0])[1]) }
                let r.lines = {
                            \ 'cur': str2float(split(status[1])[0]),
                            \ 'tot': str2float(split(status[1])[2])
                            \ }
                let r.words = {
                            \ 'cur': str2nr(split(status[2])[0]),
                            \ 'tot': str2nr(split(status[2])[2])
                            \ }
                let r.chars = {
                            \ 'cur': str2nr(split(status[3])[0]),
                            \ 'tot': str2nr(split(status[3])[2])
                            \ }
                let r.stmode = 'V'
            else " anything else; v:statusmsg =
                " Col {n} of {m}; Line {n} of {m}; Word {n} of {m};
                " \ Byte {n} of {m}
                let r.cols = {
                            \ 'cur': str2nr(split(status[0])[1]),
                            \ 'tot': str2nr(split(status[0])[3])
                            \ }
                let r.lines = {
                            \ 'cur': str2float(split(status[1])[1]),
                            \ 'tot': str2float(split(status[1])[3])
                            \ }
                let r.words = {
                            \ 'cur': str2nr(split(status[2])[1]),
                            \ 'tot': str2nr(split(status[2])[3])
                            \ }
                let r.chars = {
                            \ 'cur': str2nr(split(status[3])[1]),
                            \ 'tot': str2nr(split(status[3])[3])
                            \ }
                let r.stmode = 'n'
            endif
            let r.mode = curmode
            let r.perc = float2nr((r.lines.cur/r.lines.tot)*100)
            let r.lines.cur = float2nr(r.lines.cur)
            let r.lines.tot = float2nr(r.lines.tot)
            let v:statusmsg = s:oldstat
        endif "}}}

        call setpos('.', position)
        return r

    function! s:myperc(nr)
        if a:nr == 0
            return 'Top'
        elseif a:nr == 100
            return 'Bot'
>>>>>>> lightline
        else
            return printf('%2d%%', a:nr)
        endif
    endfunction "
    let r.myperc = s:myperc(r.perc)

    return r
endfunction "}}}
<<<<<<< HEAD
function! ToggleBackground() " {{{
    if &background=="dark"
        set background=light
        set colorcolumn=0
        set nocursorline
    else
        set background=dark
        let &colorcolumn = g:mytw
        set cursorline
=======
function! RenameFile() " {{{
    let old_name = expand('%')
    let new_name = input('New file name:', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        " only work on Linux?
        exec ':silent !rm ' . old_name
        redraw!
>>>>>>> lightline
    endif
endfunction " }}}
" --- Managing buffers, tabs, windows
function! ChTabBuf(motion) " {{{
    if tabpagenr('$') == 1
        " there is only 1 tab; switch buffers
        if a:motion < 0
            bprevious a:motion
        else
            bnext a:motion
        endif
    else
        if a:motion < 0
            tabprevious a:motion
        else
            let s:move = tabpagenr('$') - a:motion
            tabprevious s:move
        endif
endfunction " }}}
function! CloseBufWin() " {{{
    if len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) > 1
        bdelete
    else
        quit
    endif
endfunction " }}}
" --- Custom interface lines
function! FoldLine() " {{{
    let line = getline(v:foldstart)
    " Calculate widths
    let nucolwidth = &fdc + &nu * &nuw
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = printf('%3d', v:foldend - v:foldstart)
    " Replace tabs with spaces
    let onetab = strpart('        ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')
    " Get rid of fold markers
    let foldmarks = substitute(&fmr, ',.*', '', '')
    let line = substitute(line, foldmarks, '', '')
    " Get rid of commentstring
    let commentstr = substitute(&cms, '^\(.*\)%s\(.*\)', '\1\|\2', '')
    let commentstr = substitute(commentstr, '|$', '', '')
    let commentstr = substitute(commentstr, '\([\[\]\$\^\.\*|\\]\)',
                                \ '\\\1', 'g')
    let line = substitute(line, commentstr, '', 'g')
    " Replace initial whitespace with dashes indicating foldlevel
    let line = substitute(line, '^\s*', v:folddashes . ' ', '')
    " Adjust line width to fit window
    let line = strpart(line, 0, windowwidth - 2 - len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    " Output foldline
    let line = line . repeat(' ', fillcharcount)
    return line . v:foldlevel . '> ' . foldedlinecount . ' '
endfunction " }}}
function! StatusLine(winnr) " {{{
    let status = ''
    let isactive = winnr() == a:winnr
    let buffer = winbufnr(a:winnr)

    let ismodified = getbufvar(buffer, '&modified')
    let isreadonly = getbufvar(buffer, '&readonly')
    let filename = bufname(buffer)

    function! Color(isactive, group, content)
        if a:isactive && !has('win32')
            return '%' . a:group . '*' . a:content . '%*'
        else
            return a:content
        endif
    endfunction

    " Left side ===================================================
    " Column indicator
    function! Column()
        let vc = virtcol('.')
        let ruler_width = max([strlen(line('$')), (&numberwidth - 1)])
        let column_width = strlen(vc)
        let padding = ruler_width - column_width
        let column = ''

        if padding <= 0
            let column .= vc
        else
            let column .= ' ' . vc . repeat(' ', padding)
        endif
        return ' ' . column
    endfunction
    let status .= '%#CursorLineNr#'
    let status .= '%{Column()}'
    let status .= '%#StatusLine#'

    " Filename
    let status .= ' ' . Color(isactive, 4, isactive ? '»' : '›')
    let status .= ' %<'

    if filename == '__Gundo__'
        let status .= 'Gundo'
    elseif filename == '__Gundo_Preview__'
        let status .= 'Gundo Preview'
    elseif filename == ''
        let status .= '______'
    else
        let status .= '%f'
    endif

    let status .= ' ' . Color(isactive, 4, isactive ? '«' : '')

    " File status indicators
    let status .= Color(isactive, 2, ismodified ? ' + ' : '')
    let status .= Color(isactive, 2, isreadonly ?
                \ &ft == 'help' ? ' ? ' : ' ‼ '
                \ : '')
    if isactive && &paste
        let status .= '%2*' . ' P ' . '%*'
    endif

    let status .= '%=' " Gutter ====================================

    " Right side ===================================================
    let status .= '%p%%'

    return status
endfunction " }}}
function! s:RefreshStatus() " {{{
    for nr in range(1, winnr('$'))
        call setwinvar(nr, '&statusline', '%!StatusLine('.nr.')')
    endfor
endfunction " }}}
" function! TabLine() " {{{
" endfunction " }}}
" Custom theming
function! UpdateCursorLineNumber() " {{{
    if &bg == 'dark'
        hi CursorLineNr guifg=#6c71c4 gui=bold
    endif
endfunction " }}}
" }}}
<<<<<<< HEAD
" Part III: Better ViM defaults {{{
" because vanilla vim, though great, is still lacking.
syntax on                      " syntax highlighting is great
set number                     " and line numbers, too
set relativenumber             " and relative numbers on non-this line
set autoread                   " reload on a change, automagically
set lazyredraw                 " don't redraw macros til done
set hidden                     " Don't close unused buffers

set formatoptions-=ro          " disable autocomments in (I)
set history=1000               " set history length
set backspace=indent,eol,start " backspace across these things

set encoding=utf-8             " encoding = utf-8.
set fileencoding=utf-8         " because year = 2014.
set fileformat=unix            " Unix file ending default
set fileformats=unix,dos       " but recognize DOS line endings

set viminfo='100               " Save marks for 100 files max
set viminfo^=!                 " Save global all-caps variables
set viminfo+=<50               " Save 50 lines of registers
set viminfo+=s10               " Save only first 10 Kb of each register
set viminfo+=h                 " Disable 'hlsearch' on saved files
"}}}
" Part IV:  Customization {{{
" --- Appearance {{{
set background=dark            " Dark background (duh)
colorscheme solarized
let &colorcolumn = g:mytw
set cursorline             " highlight the line the cursor's on
=======
" PLUGINS {{{
" Vundle {{{
filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()
>>>>>>> lightline

Plugin 'gmarik/Vundle.vim'            " let Vundle manage Vundle

" MAKING WRITING EASIER
Plugin 'junegunn/goyo.vim'            " distraction-free writing
Plugin 'junegunn/limelight.vim'       " highlight current paragraph
Plugin 'chrisbra/NrrwRgn'             " Open region in new win to edit
Plugin 'nelstrom/vim-visual-star-search' " Use * or # from V-Block

" COLORS & EYECANDY
Plugin 'junegunn/seoul256.vim'
Plugin 'altercation/vim-colors-solarized'
" Plugin 'itchyny/lightline.vim'
" Plugin 'bling/vim-airline'            " a better statusline

" FORMATTING TEXT
Plugin 'godlygeek/tabular'            " Easy aligning of text
Plugin 'junegunn/vim-easy-align'      " Vim alignment plugin
Plugin 'Lokaltog/vim-easymotion'      " No more counting objects
Plugin 'osyo-manga/vim-over'          " Preview :s/ searches
Plugin 'AndrewRadev/splitjoin.vim'    " Easily split and join code structs
Plugin 'wellle/targets.vim'           " Lots of new textobjects
Plugin 'michaeljsmith/vim-indent-object' " Provides a textobj for indentblocks
" [ by TIM POPE ]
Plugin 'tpope/vim-abolish'            " Enhanced search and replace
Plugin 'tpope/vim-commentary'         " Easier commmenting
Plugin 'tpope/vim-repeat'             " Repeat plugin commands with .
Plugin 'tpope/vim-speeddating'        " <C-a>,<C-x> on dates and times
Plugin 'tpope/vim-surround'           " Format surroundings easily
Plugin 'tpope/vim-endwise'            " Auto-add 'end*'s in code
Plugin 'tpope/vim-characterize'       " Modernize `ga` behavior
" FILESYSTEM
Plugin 'kien/ctrlp.vim'               " A fuzzy file finder
Plugin 'dockyard/vim-easydir'         " Create new dirs on-the-fly

Plugin 'xolox/vim-shell'              " Integrate ViM and environment
Plugin 'xolox/vim-misc'               " Required by vim-shell

" FILETYPES
Plugin 'mattn/emmet-vim'              " Zencoding for HTML
Plugin 'gregsexton/MatchTag'          " Match HTML tags with %
Plugin 'hail2u/vim-css3-syntax'       " syntax file for CSS3

Plugin 'vim-pandoc/vim-pandoc'        " Pandoc helpers
Plugin 'vim-pandoc/vim-pandoc-syntax' " Pandoc syntax

Plugin 'vimwiki/vimwiki'              " Personal wiki with ViM

Plugin 'sheerun/vim-polyglot'         " Many syntax defs

" PLUGINS THAT REQUIRE THINGS
if executable('git')
    Plugin 'tpope/vim-fugitive'       " Git integration
    Plugin 'airblade/vim-gitgutter'   " Git stuff in signs column
endif
if executable('ag')
    Plugin 'rking/ag.vim'             " Ag implementation
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g "" '
endif
if executable('diff')
    Plugin 'mbbill/undotree'          " Visualize Vim's undo tree
    nnoremap <F5> :UndotreeToggle<CR>
elseif has('python')
    Plugin 'vim-scripts/gundo'        " Visualize Vim's undo tree
    nnoremap <F5> :GundoToggle<CR>
endif

call vundle#end()                     " req'd
filetype plugin indent on             " req'd
"}}}
" Plugin settings {{{
" Ag
let g:agprg = 'ag --column --smart-case'
let g:aghighlight = 1 " highlight searches
" Ctrl-P
let g:ctrlp_map = '<C-p>'
let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_cache_dir = $HOME . '/.vim/cache/ctrlp'
let g:ctrlp_max_depth = 100
let g:ctrlp_max_files = 0
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_lazy_update = 1
let g:ctrlp_extensions = [ 'dir', 'line', 'mixed', ]
" EasyMotion
let g:EasyMotion_do_mapping       = 0 " disable default mappings
let g:EasyMotion_prompt           = '{n}/>> ' " prompt
let g:EasyMotion_keys             = 'asdfghjkl;qwertyuiopzxcvbnm' " hints
let g:EasyMotion_smartcase        = 1 " case-insensitive searches
let g:EasyMotion_use_smartsign_us = 1 " 'case'-insensitive number row
let g:EasyMotion_enter_jump_first = 1 " <CR> jumps to first hint
" Emmet
let g:user_emmet_leader_key = '<C-e>'
" Goyo
let g:goyo_width         = g:tw
let g:goyo_margin_top    = 2
let g:goyo_margin_bottom = g:goyo_margin_top
" Gundo
let g:gundo_preview_bottom = 1 " show preview below all windows
let g:gundo_auto_preview   = 1 " default; toggle to speed up Gundo
" Lightline
let g:lightline = {}
let g:lightline.colorscheme = 'solarized'
let g:lightline.mode_map = {
            \   'n' : 'Nr', 
            \   'i' : 'In',
            \   'R' : 'Re',
            \   'v' : 'Vi',
            \   'V' : 'V_',
            \   '': 'V[',
            \   'c' : 'Co',
            \   's' : 'Se',
            \   'S' : 'S_',
            \   '': 'S[',
            \   '?' : '--',
            \   }
let g:lightline.separator = { 'left': '', 'right': '' }
let g:lightline.subseparator = { 'left': '|', 'right': '|' }
let g:lightline.active = {
            \ 'left' : [
            \            [ 'col-fn', 'ispaste' ],
            \            [ 'filename', 'filestatus' ],
            \          ],
            \ 'right': [
            \            [ 'lineinfo' ],
            \            [ 'percent'  ],
            \            [ 'filetype' ],
            \          ],
            \ }
let g:lightline.inactive = {
            \ 'left' : [
            \            [ 'filename' ],
            \          ],
            \ 'right': [
            \            [ 'lineinfo' ],
            \            [ 'percent' ],
            \          ],
            \ }
let g:lightline.component = { 'filetype' : '%y' }
let g:lightline.component_function = {
            \   'col-fn': 'ColumnOrFilename',
            \   'ispaste': 'IsPaste',
            \   'filestatus': 'FileStatus',
            \   'filename': 'MyFilename',
            \ }
" Solarized
let g:solarized_menu = 0
" Splitjoin
let g:splitjoin_split_mapping = 'gK'
" Undotree
let g:undotree_WindowLayout = 2
let g:undotree_DiffAutoOpen = 1
let g:undotree_SetFocusWhenToggle = 1
" Vim-shell
let g:shell_mappings_enabled   = 0 " disable default mappings
let g:shell_fullscreen_message = 0 " don't help me to get out of fullscreen
" Vimwiki
let g:vimwiki_hl_headers = 0 " enable different colored headers
let g:vimwiki_hl_cb_checked = 1 " hilight [X] with Comment
" }}}
" Plugin keymaps {{{
nnoremap <F11> :Goyo<CR>
nnoremap <S-F11> :Fullscreen<CR>

<<<<<<< HEAD
set backupcopy=yes  " copy the original and overwrite
set backup          " Keep backup files, just in case

set noerrorbells      " don't beep on errors
set visualbell        " flash instead of beeping

set expandtab         " use spaces instead of tabs
set autoindent        " indent based on filetype and previous line
set smartindent       " slightly smarter than auto (for C)
set shiftwidth=4      " when reading, tabs=4 spaces
set softtabstop=4     " in insert mode, tabs=4 spaces
set shiftround        " indent to nearest tabstop

set incsearch         " find the next match as we type
set hlsearch          " highlight search matches
set ignorecase        " ignore case as we search
set smartcase         " ... unless a capital appears

set foldenable        " Enable folding
set foldmethod=marker " {{{ }}} mark folds
set foldlevel=2       " Start open to second level
set foldcolumn=0      " Fold columns in gutter
set foldtext=MyFoldText() " custom function (above)

set gdefault          " default to global (line) substitutions
set magic             " use better regexp

" let &textwidth = g:mytw
" --- }}}
" --- Keybinds {{{
" --- --- Keybinds for usability {{{
" <Shift> AND ; is SO MUCH WORK
noremap ; :
" j and k should work on visual lines, not code lines
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
" H, L go to beginning, end of line
nnoremap H ^
nnoremap L $
" Y should do similar things as C or D
nnoremap Y y$
" ESC is so far away...
inoremap jj <Esc>
" <Space> to open AND close folds
nnoremap <Space> za
vnoremap <Space> za
nnoremap <S-Space> zA
vnoremap <S-Space> zA
nnoremap <C-Space> zM
nnoremap <C-S-Space> zR
" make K the opposite of J (split lines at cursor)
nnoremap K i<CR><Esc>
" More useful <F1> bind -- looks up :h <cursor>
nnoremap <F1> K
" Map Q (usu. for Ex mode LAME) to closing the buffer (or window)
nnoremap Q :call CloseBufferOrWindow()<CR>
" allow Tab and Shift+Tab to change selection indent in visual mode
vnoremap <leader>> >gv
vnoremap <leader>< <gv
" \ does netrw window
nnoremap \ :Explore<CR>
" gs to begin a search
nnoremap gs :%s/
xnoremap gs :s/
" Map dD to remove line but leave blank
nnoremap dD ddO<Esc>
" :w!! to sudo save file (Linux only)
cmap w!! %!sudo tee > /dev/null %
" --- --- }}}
" --- --- Keybinds for window management {{{
" --- --- --- Switching windows
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
nnoremap <S-UP>    <C-w>K
nnoremap <S-DOWN>  <C-w>J
nnoremap <S-LEFT>  <C-w>H
nnoremap <S-RIGHT> <C-w>L
nnoremap <C-UP>    <C-w>+
nnoremap <C-DOWN>  <C-w>-
nnoremap <C-LEFT>  <C-w>>
nnoremap <C-RIGHT> <C-w><
" --- --- }}}
" --- --- Leader binds {{{
let mapleader = ","
" Easily edit $MYVIMRC
nnoremap <leader>ev :edit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
" Change working directory to that of current buffer
nnoremap <leader>cd :cd %:p:h<CR>
=======
vnoremap \| :Tabularize /
>>>>>>> lightline

nnoremap gS :%S/
vnoremap gS :S/

nnoremap <C-P> :CtrlPMRU<CR>

nmap f <Plug>(easymotion-sl)
nmap t <Plug>(easymotion-bd-tl)
omap f <Plug>(easymotion-sl)
omap t <Plug>(easymotion-bd-tl)
nmap F <Plug>(easymotion-s)
nmap T <Plug>(easymotion-bd-t)
omap F <Plug>(easymotion-s)
omap T <Plug>(easymotion-bd-t)
nmap <Leader>; <Plug>(easymotion-next)
nmap <Leader>, <Plug>(easymotion-prev)
"}}}
" Plugin functions {{{
" ‼‹›℗←↑→↓◊∫∂↔↕Ω˂˃˄˅§«»±¶¤Ππ‘’“”⌂
function! ColumnOrFilename()
    let s:fname = expand('%:t')
    if s:fname =~ '__Gundo'
        let s:corf = ' ±'
    elseif s:fname =~ 'ControlP'
        let s:corf = ' π'
    elseif s:fname =~ 'NrrwRgn_'
        let s:corf = ' ↕'
    elseif &ft == 'netrw'
        let s:corf = ' ⌂'
    else
        let s:corf = winwidth(0) > 60
                    \ ?
                    \   col('.') / 100 >= 1
                    \   ? printf('%d', col('.'))
                    \   : printf('%2d', col('.'))
                    \ : ''
    endif
    return s:corf
endfunction
function! IsPaste()
    if &paste
        return 'P'
    else
        return ''
    endif
endfunction
function! FileStatus()
    if &ft =~? 'help'
        return '?'
    elseif &readonly
        return '‼'
    elseif &modified
        return '+'
    else
        return ''
    endif
endfunction
function! MyFilename()
    let fname = expand('%:t')
    if fname =~ '__Gundo' || fname =~ 'ControlP' || fname =~ 'NrrwRgn_' ||
                \ &ft == 'netrw'
        return ''
    elseif len(fname) == 0
        return '[_]'
    else
        return fname
    endif
endfunction
" }}}
" Plugin autocommands {{{
augroup GoyoEvents
    au!
<<<<<<< HEAD
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * if !Typewriter('is?') |
                              \ set cursorline | endif
augroup END "}}}
augroup Trailing "{{{
    " Only show trailing spaces when out of insert mode
    au!
    au InsertEnter * :match none '\s\+$'
    au InsertLeave * :match Error '\s\+$'
augroup END " }}}
augroup ft_help "{{{
    au!
    au FileType help setlocal nospell
    au BufWinEnter *.txt
                \ if &ft == 'help' && &columns >= 156 |
                \     wincmd L |
                \ endif
augroup END "}}}
" }}}
" Part VI:  Can I has() options? {{{
if has('mouse')
    set mouse=a       " Enable the mouse if present
endif
if has('gui_running') " --- GVIM {{{
    " --- Common GUI options
    set guioptions-=m  " remove menu bar
    set guioptions-=T  " remove toolbar
    set guioptions-=r  " remove right-hand scroll
    set guioptions-=L  " remove left-hand scroll
    set guioptions-=e  " remove GUI tabline; use consoley one instead
    augroup OpenVimWithCustomSize
        au!
        au VimEnter * let &columns = g:mytw + &fdc + &nu * &nuw
        au VimEnter * set lines=36
    augroup END
    " --- Set fonts for different systems
    if has("gui_gtk2")
        set guifont=Inconsolata
        let g:airline_powerline_fonts = 1
    elseif has("x11") " also works with GTK 1
        "set guifont=*-lucidatypewriter-medium-r-normal-*-*-180-*-*-m-*-*
    elseif has("gui_win32")
        set guifont=Consolas:h11:cANSI
        " Consolas don't have powerline arrows
    endif
endif
" --- }}}
if has('win32') " --- WINDOWS {{{
    let &runtimepath.=',$HOME/.vim' " for portability

    " define keymap for font size change
    nnoremap <C-Up> :silent! let &guifont = substitute(
                \ &guifont,
                \ ':h\zs\d\+',
                \ '\=eval(submatch(0)+1)',
                \ '')<CR>
    nnoremap <C-Down> :silent! let &guifont = substitute(
                \ &guifont,
                \ ':h\zs\d\+',
                \ '\=eval(submatch(0)-1)',
                \ '')<CR>

    " --- Plugins
    " Ctrl-P starts in D:\Dropbox
    let g:ctrlp_cmd       = 'CtrlP D:\Dropbox\'
    "let g:ctrlp_cache_dir = 'D:\Dropbox\apps\ctrlp'
    nnoremap \ :CtrlP C:\Users\Case<CR>
    " No python support :(?)
    let g:pandoc#modules#disabled = ["bibliographies"]

    " --- Windows like clipboard / saving
    " yank to and paste from the clipboard without prepending "*
    let &clipboard = has('unnamedplus') ? 'unnamedplus' : 'unnamed'
    " Cut and copy mappings (V)
    vmap <c-x> "+x
    vmap <c-c> "+y
    " Paste mappings (C) (I)
    cnoremap <c-v> <c-r>+
    exe 'ino <script> <C-V>' paste#paste_cmd['i']
    " save with c-s (N) (I)
    nmap <c-s> :w<CR>
    imap <c-s> <Esc>:w<CR>a

    set viminfo+=rA:,rB: " Don't store marks for A: or B:
=======
    au User GoyoEnter Limelight
    au User GoyoEnter set nocursorline

    au User GoyoLeave Limelight!
    au User GoyoLeave set cursorline
augroup END
"}}}
" Plugin can I has(?) {{{
" Create directories if not exist
if !isdirectory(expand(g:ctrlp_cache_dir))
    call mkdir(expand(g:ctrlp_cache_dir), 'p')
>>>>>>> lightline
endif

let g:ctrlp_mruf_case_sensitive = has('win32') ? 0 : 1
" }}}
"}}}

colorscheme solarized
