" ______________________ VIMRC -- CASE DUCKWORTH _________________________
" vim:foldlevel=0:textwidth=0:nowrap:nolinebreak
set nocompatible
" Part 0: Vundle {{{
filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'            " let Vundle manage Vundle

Plugin 'bling/vim-airline'            " a better statusline
Plugin 'junegunn/goyo.vim'            " distraction-free writing
Plugin 'junegunn/limelight.vim'       " highlight only active para
Plugin 'reedes/vim-colors-pencil'     " pencil colorscheme
Plugin 'nice/sweater'                 " light scheme

Plugin 'tpope/vim-repeat'             " repeat plugin commands with
Plugin 'tpope/vim-surround'           " format surroundings easily
Plugin 'tpope/vim-abolish'            " Enhanced search and replace
Plugin 'tpope/vim-speeddating'        " C-a C-x on dates and times
Plugin 'tpope/vim-commentary'         " commenting

Plugin 'Lokaltog/vim-easymotion'      " No more counting objects
Plugin 'reedes/vim-textobj-sentence'  " Improved sentence textobj

Plugin 'dockyard/vim-easydir'         " Create new dirs on-the-fly
Plugin 'chrisbra/NrrwRgn'             " Open region in new win to ed
Plugin 'kien/ctrlp.vim'               " a fuzzy finder
Plugin 'godlygeek/tabular'            " easy formatting of text tabs

Plugin 'mattn/emmet-vim'              " Zencoding for HTML
Plugin 'gregsexton/MatchTag'          " Match HTML tags with %
Plugin 'vim-pandoc/vim-pandoc'        " Pandoc helpers
Plugin 'vimwiki/vimwiki'              " Personal wiki with ViM

Plugin 'sheerun/vim-polyglot'         " Many syntax defs
Plugin 'hail2u/vim-css3-syntax'       " syntax file for CSS3
Plugin 'vim-pandoc/vim-pandoc-syntax' " Pandoc syntax

Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-shell'

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

"Plugin 'chriskempson/base16-vim'   " base 16 colors
"Plugin 'q335r49/microviche'        " infinite pannable vim
"Plugin 'scrooloose/nerdcommenter'  " toggle comments easily
"Plugin 'AndrewRadev/splitjoin.vim' " Split and join code easily
"Plugin 'ervandew/supertab'         " tab completion in (I)

call vundle#end()                      "req'd
filetype plugin indent on              "req'd
"}}}
" Part I: Plugin Config {{{

let g:shell_mappings_enabled  = 0 " Disable vim-shell mappings
let g:shell_fullsreen_message = 0 " I know what I'm doing

if &textwidth " Use textwidth if defined; else use 78
    let g:goyo_width = &textwidth
else
    let g:goyo_width = 78
endif
let g:goyo_margin_top = 2
let g:goyo_margin_bottom = 2

let g:gundo_preview_bottom = 1 " Preview takes up full width

let g:EasyMotion_do_mapping = 0 " Disable Easymotion default mappings
let g:EasyMotion_prompt = '{n}/>> '
let g:EasyMotion_keys = 'asdfghjkl;qwertyuiopzxcvbnm'
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
let g:airline_section_b  = "%<%f%{&modified ? ' +' : ''}"
let g:airline_section_b .= "%{&readonly ? ' !!' : ''}"

let g:airline_section_c  = ""

let g:airline_section_y  = '%2p%%'            " pp%

let g:airline_section_z  = '_%02l'            " _ll
let g:airline_section_z .= '|%02c'            " |cc

let g:airline#extensions#default#section_truncate_width = {
    \ 'b': 40,
    \ 'x': 60,
    \ 'y': 68,
    \ 'z': 45,
    \ }

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

let g:airline#extensions#tabline#enabled         = 1
let g:airline#extensions#tabline#fnamemod        = ':t'
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#ctrlp#show_adjacent_modes = 0
let g:airline#extensions#quickfix#quickfix_text = 'Qf'
let g:airline#extensions#quickfix#location_text = 'Lc'

let g:airline#extensions#whitespace#trailing_format     = 'tw[%s]'
let g:airline#extensions#whitespace#mixed_indent_format = 'mi[%s]'

" --- }}}
" }}}
" Part II: Custom functions {{{
function! Count(thing) " {{{ Count(words|bytes|thisw|thisb)
    "character counting:
    let s:old_status = v:statusmsg
    let position = getpos(".")
    exe ":silent normal g\<c-g>"
    let stat = v:statusmsg
    let s:word_count = 0
    if stat != '--No lines in buffer--'
        let s:word_count = str2nr(split(stat)[11])
        let s:word_curnt = str2nr(split(stat)[9])
        let s:byte_count = str2nr(split(stat)[15])
        let s:byte_curnt = str2nr(split(stat)[13])
        let v:statusmsg = s:old_status
    endif
    call setpos('.', position)
    if a:thing == 'words'
        return s:word_count
    elseif a:thing == 'bytes'
        return s:byte_count
    elseif a:thing == 'thisw'
        return s:word_curnt
    elseif a:thing == 'thisb'
        return s:byte_curnt
    endif
endfunction "}}}
function! ToggleBackground() " {{{
    if &background=="dark"
        set background=light
        set colorcolumn=0
        set nocursorline
    else
        set background=dark
        if &textwidth
            let &colorcolumn = &textwidth
        else
            set colorcolumn=78
        endif
        set cursorline
    endif
endfunction " }}}
function! NextTabOrBuffer(dir) " {{{
    " TODO: add count
    " if there's only one tab, switch bufs; else switch tabs
    if tabpagenr('$') == 1 "there's only one 'tab' so switch bufs
        if a:dir < 0 " negative numbers go previous
            bprevious
        else
            bnext
        endif
    else " real tabs exist
        if a:dir < 0
            tabprevious
        else
            tabnext
        endif
    endif
endfunction " }}}
function! CloseBufferOrWindow() " {{{
    if len(filter(range(1, bufnr("$")), "buflisted(v:val)")) > 1
        bdelete
    else
        quit
    endif
endfunction " }}}
" }}}
" Part III: Better ViM defaults {{{
" because vanilla vim, though great, is still lacking.

syntax on                      " syntax highlighting is great
set number                     " and line numbers, too
set autoread                   " reload on a change, automagically
set lazyredraw                 " don't redraw macros til done
set hidden                     " Don't close unused buffers

set formatoptions-=ro          " disable autocomments in (I)
set history=1000               " set history of commands to 1000 long.
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
" Part IV: Customization {{{
" --- Appearance {{{
set background=dark            " Dark background (duh)
if has('gui_running') || &t_Co>=88
    colorscheme pencil
    set colorcolumn=78         " highlight column 78
    set cursorline             " highlight the line the cursor's on
else                           " 8-color terms can't handle colors
    colorscheme desert
    set colorcolumn=0
    set nocursorline
endif

set laststatus=2               " use status line, always.

" Statusline
set statusline=\>\ b%n              " buffernumber
set statusline+=\>\ %f              " basename of file
set statusline+=%m                  " modified flag [+] or [-] if ro
set statusline+=%h                  " help buffer flag [help]
set statusline+=%=                  " begin right-align
set statusline+=\<%y\               " file type
set statusline+=\<%3p%%)            " scroll percentage
set statusline+=_%02l               " current line / total lines
set statusline+=\|%02c              " current column
"set statusline+={%{Count('words')}w " word count function

set wildmenu           " tab completion with a menu
set ruler              " show ruler
set showcmd            " Show partial commands as-you-type

match ErrorMsg '\s\+$' " Show trailing whitespace as error

set scrolloff=8        " keep lines at bottom and top when scrolling
set sidescrolloff=4    " keep lines at left and right when scrolling
set sidescroll=1       " scroll sideways by characters, not screens
set wrap               " set wrapping
set linebreak          " wrap at words. (:help breakat)

set splitbelow         " split new windows below
set splitright         " ... and at the right

" Spelling
hi SpellBad gui=undercurl
" --- }}}
" --- Acting {{{
set directory=$HOME/.vim/swap//    " directory for swap files
set backupdir=$HOME/.vim/backup//  " directory for backups
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

set magic             " use better regexp
" --- }}}
" --- Keybinds {{{
" --- --- Keybinds for usability {{{
" \ is a dumb mapleader.
let mapleader = ","
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
" Easily edit $MYVIMRC
nnoremap <leader>ev :edit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
" Change working directory to that of current buffer
nnoremap <leader>cd :cd %:p:h<CR>

" remove search highlight
nnoremap <leader>/ :nohlsearch<return><Esc>

" Remove whitespace from the ends of lines
nnoremap <leader>rs :%s/\s\+$//e<CR>
" Remove blank lines
noremap <leader>rb :g/^$/d<CR>
" --- --- }}}
" --- --- Function keybinds {{{
" --- --- --- Count() {{{
nnoremap <leader>wc :echo 'words: ' . Count("words")<CR>
nnoremap <leader>swc :let g:airline_section_y =
                   \'%{Count("words")}w (%2p%%)'<CR>
                   \:AirlineRefresh<CR>
nnoremap <leader>sbc :let g:airline_section_y =
                   \'%{Count("bytes")}b (%2p%%)'<CR>
                   \:AirlineRefresh<CR>
nnoremap <leader>stw :let g:airline_section_y =
                   \'%{Count("thisw")}/%{Count("words")}w (%2p%%)'<CR>
                   \:AirlineRefresh<CR>
nnoremap <leader>stb :let g:airline_section_y =
                   \'%{Count("thisb")}/%{Count("bytes")}b (%2p%%)'<CR>
                   \:AirlineRefresh<CR>
nnoremap <leader>s% :let g:airline_section_y =
                   \'%2p%%'<CR>
                   \:AirlineRefresh<CR>
" --- --- --- }}}
nnoremap <F6> :call ToggleBackground()<CR>
nnoremap gt :call NextTabOrBuffer(1)<CR>
nnoremap gT :call NextTabOrBuffer(-1)<CR>
" --- --- }}}
" --- --- Plugin keybinds {{{
nnoremap <F11>   :Goyo<CR>
nnoremap <S-F11> :Fullscreen<CR>
nnoremap <F5>    :GundoToggle<CR>
vnoremap \|      :Tabularize /
let g:user_emmet_leader_key = '<c-e>'

nmap f <Plug>(easymotion-bd-f)
nmap t <Plug>(easymotion-bd-t)
omap f <Plug>(easymotion-bd-f)
omap t <Plug>(easymotion-bd-t)
nmap <Leader>, <Plug>(easymotion-repeat)
" --- --- }}}
" --- }}}
" }}}
" Part V: Autocommands {{{
" Change local current directory to buffer's directory
autocmd BufEnter * silent! lcd %:p:h
augroup GoyoEvents " Goyo fullscreens
    autocmd!
    autocmd User GoyoEnter Limelight
    autocmd User GoyoLeave Limelight!
augroup END
augroup TextEditing " For texty filetypes
    autocmd!
    autocmd BufNewFile,BufRead *.md set ft=markdown spell
    autocmd FileType vimwiki,markdown,text setlocal spell
    " TODO: switch back when not a texty filetype
    autocmd FileType help setlocal nospell
augroup END
augroup ReloadVimrc " Source $MYVIMRC on save
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC | AirlineRefresh
augroup END
augroup AirlineShowmode " Toggle showmode for enter/exit airline
    autocmd!
    autocmd User AirlineToggledOn set noshowmode
    autocmd User AirlineToggledOff set showmode
augroup END
" }}}
" Part VI: Can I has() options? {{{
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
    set columns    =78
    set lines      =36
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
endif
" --- }}}
" }}}
