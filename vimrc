" _______________ VIMRC -- CASE DUCKWORTH _________________
" vim:foldlevel=0:textwidth=0:nowrap:nolinebreak
set nocompatible
" Part 0: Vundle {{{
filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()
if exists(":Plugin")
    Plugin 'gmarik/Vundle.vim'         " let Vundle manage Vundle, req'd
    " Plugins
    " --- Using ViM {{{
    Plugin 'Lokaltog/vim-easymotion'   " No more counting objects
    Plugin 'chrisbra/NrrwRgn'          " Open region in new win to edit
    Plugin 'vim-scripts/gundo'         " Visualize Vim's undo tree
    " --- --- VIM eyecandy
    Plugin 'bling/vim-airline'         " a better statusline
    Plugin 'junegunn/goyo.vim'         " distraction-free writing
    Plugin 'junegunn/limelight.vim'    " highlight only active paragraph
    " --- --- Colorschemes
    Plugin 'reedes/vim-colors-pencil'  " pencil colorscheme
    Plugin 'chriskempson/base16-vim'   " base 16 colors
    " --- --- Vim Wiki
    Plugin 'vimwiki/vimwiki'
    " --- --- Git integration
    Plugin 'airblade/vim-gitgutter'        " Git stuff in signs column
    Plugin 'tpope/vim-fugitive'            " Git integration
    " --- --- Shell integration/misc.
    Plugin 'xolox/vim-misc'
    Plugin 'xolox/vim-shell'
    " --- }}}
    " --- Editing Files {{{
    " --- --- Navigating and saving
    Plugin 'scrooloose/nerdtree'       " an easy-to-use file manager
    Plugin 'kien/ctrlp.vim'            " a fuzzy finder
    "Plugin 'mhinz/vim-startify'        " start page with recent files
    Plugin 'dockyard/vim-easydir'      " Create new dirs on-the-fly
    " --- --- Working within files
    Plugin 'scrooloose/nerdcommenter'  " toggle comments easily
    Plugin 'tpope/vim-surround'        " format surroundings easily
    Plugin 'godlygeek/tabular'         " easy formatting of text tables
    Plugin 'ervandew/supertab'         " tab completion in (I)
    Plugin 'AndrewRadev/splitjoin.vim' " Split and join code easily
    Plugin 'tpope/vim-abolish'         " Enhanced search and replace
    Plugin 'q335r49/microviche'        " infinite pannable vim
    " --- --- Filetypes
    " --- --- Plain text
    Plugin 'reedes/vim-textobj-sentence' " Improved sentence textobj
    " --- --- --- HTML
    Plugin 'mattn/emmet-vim'           " Zencoding for HTML
    Plugin 'gregsexton/MatchTag'       " Match HTML tags with %
    " --- --- --- CSS
    "Plugin 'hail2u/vim-css3-syntax'    " syntax file for CSS3
    " --- --- --- Pandoc
    Plugin 'vim-pandoc/vim-pandoc'
    Plugin 'vim-pandoc/vim-pandoc-syntax'
    " --- --- --- Polyglot: over fifty languages' syntax files
    Plugin 'sheerun/vim-polyglot'
    " --- }}}
endif
call vundle#end()                      "req'd
filetype plugin indent on              "req'd
"}}}
" Part I: Plugin Config {{{

let g:shell_mappings_enabled = 0 " Disable vim-shell mappings
let g:shell_fullsreen_message = 0 " I know what I'm doing

" Goyo maximizes window
augroup Goyo_events         " Call these functions
    autocmd!
    autocmd User GoyoEnter Fullscreen | Limelight | sleep 50m | Goyo g:goyo_width
    autocmd User GoyoLeave Fullscreen | Limelight!
augroup END

" map emmet (ZenCoding) to <C-E>
let g:user_emmet_leader_key = '<c-e>'

" Ctrl-P settings
if executable('ag') " use The Silver Searcher if it exists
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif
let g:ctrlp_max_depth = 100         " max depth of search
let g:ctrlp_max_files = 0           " no limit to how many files
let g:ctrlp_use_caching = 1         " enable caching
let g:ctrlp_clear_cache_on_exit = 0 " enable cross-session caching
let g:ctrlp_cache_dir = $HOME.'/.cache/ctrlp'

" AIRLINE OPTIONS
let g:airline_section_z = '%2p%%)_%2l|%2c{%{WordCount()}w'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
if exists('*airline#add_statusline_func')
    AirlineRefresh
    set noshowmode         " Airline already shows mode, not necessary
endif

let g:markdown_fold_style = 'nested' " Fold Markdown on 

let g:goyo_width = 72           " Goyo width of 72 characters
let g:goyo_margin_top = 4       " Margins above and below Goyo window
let g:goyo_margin_bottom = 4

let g:gundo_preview_bottom = 1 " Preview takes up full width
" }}}
" Part II: Custom functions {{{
function! WordCount() " {{{
    " TODO: Generalize to count words, characters, etc.
    "character counting:
    "strwidth(join(getline(1,"$")))
    let s:old_status = v:statusmsg
    let position = getpos(".")
    exe ":silent normal g\<c-g>"
    let stat = v:statusmsg
    let s:word_count = 0
    if stat != '--No lines in buffer--'
        let s:word_count = str2nr(split(v:statusmsg)[11])
        let v:statusmsg = s:old_status
    endif
    call setpos('.', position)
    return s:word_count
endfunction "}}}
function! ToggleBackground() " {{{
    if &background=="dark"
        set background=light
    else
        set background=dark
    endif
endfunction " }}}
function! NextTabOrBuffer(dir) " {{{
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
" TODO: a function that switches between basename and full path in stl
" }}}
" Part III: Better ViM defaults {{{
" because vanilla vim, though great, is still lacking.
" and because we don't need sensible.vim! YEAH
" --- Display
syntax on                      " syntax highlighting is great
set number                     " and line numbers, too
set autoread                   " reload on a change, automagically
set lazyredraw                 " don't redraw macros til done
set hidden                     " Don't close unused buffers
" --- Using Vim
set formatoptions-=ro          " disable autocomments in (I)
set history=1000               " set history of commands to 1000 long.
runtime macros/matchit.vim     " enable matchit plugin (better %)
set backspace=indent,eol,start " backspace across these things
" --- File encoding and format
set encoding=utf-8             " encoding = utf-8.
set fileencoding=utf-8         " because year = 2014.
set fileformat=unix            " Unix file ending default
set fileformats=unix,dos       " but recognize DOS line endings
" --- Viminfo file
set viminfo='100               " Save marks for 100 files max
set viminfo^=!                 " Save global all-caps variables
set viminfo+=<50               " Save 50 lines of registers
set viminfo+=s10               " Save only first 10 Kb of each register
set viminfo+=h                 " Disable 'hlsearch' on saved files
"}}}
" Part IV: Customization {{{
" --- Appearance {{{

set background=dark    " Dark background (duh)
if has("gui_running") || &t_Co>=88
    colorscheme base16-default     " fun bright colors
    set colorcolumn=72     " highlight column 80
    set cursorline         " highlight the line the cursor's on
else                       " 8-color terms can't handle colors
    colorscheme desert
    set colorcolumn=
    set nocursorline
endif

set laststatus=2       " use status line, always.

" Statusline
set statusline=%t                " basename of file
set statusline+=%H               " help buffer flag ,HLP
set statusline+=%R               " read-only flag ,RO
set statusline+=\ %m             " modified flag [+]
set statusline+=%=               " begin right-align
set statusline+=%y\              " file type
set statusline+=%3p%%)           " scroll percentage
set statusline+=_%03l             " current line / total lines
set statusline+=\|%02c            " current column
set statusline+={%{WordCount()}w " word count function

set wildmenu           " tab completion with a menu
set ruler              " show ruler
set showcmd            " Show partial commands as-you-type

match ErrorMsg '\s\+$' " Show trailing whitespace as error

set scrolloff=8        " keep lines at bottom and top when scrolling
set sidescrolloff=4    " keep lines at left and right when scrolling
set sidescroll=1       " scroll sideways by characters, not screens
set wrap               " set wrapping
set linebreak          " wrap at words. (:help breakat)

" Spelling
autocmd FileType vimwiki,markdown,text setlocal spell
autocmd FileType help setlocal nospell
hi SpellBad gui=undercurl
" --- }}}
" --- Acting {{{
set writebackup       " save a backup before writing
set nobackup          " but don't keep the backup file

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

if has('mouse')
    set mouse=a       " Enable the mouse if present
end
" --- }}}
" --- Keybinds {{{
let mapleader = "," " \ is a dumb mapleader.

" allow Tab and Shift+Tab to change selection indent in visual mode
vmap <Tab> >gv
vmap <S-Tab> <gv

" ; = : and : = ;
noremap ; :
"noremap : ;
" j and k should work on visual lines, not code lines
nnoremap j gj
nnoremap k gk
" make K the opposite of J (split lines at cursor)
nnoremap K i<CR><Esc>
" Y should do similar things to C or D
nnoremap Y y$
" ESC is so far away...
inoremap jj <Esc>

" Turns out, K is useful -- remap (using h for help)
nnoremap <F1> K

" Make windows easier to navigate
map <C-j> <C-w>j<C-w>_
map <C-k> <C-w>k<C-w>_
map <C-h> <C-w>h<C-w>_
map <C-l> <C-w>l<C-w>_
" TODO: more window binds, eh?

" Easily edit $MYVIMRC
nnoremap <leader>ev :edit $MYVIMRC<CR>
" Source $MYVIMRC on save
augroup reload_vimrc
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

" use <Space> to remove search highlight
nnoremap <leader><Space> :nohlsearch<return><Esc>
" Remove whitespace from the ends of lines
nnoremap <leader>r<Space> :%s/\s\+$//e<CR>

" --- --- Function keybinds
nnoremap <leader>wc :echo 'words: '.WordCount()<CR>
nnoremap <F6> :call ToggleBackground()<CR>
nnoremap gt :call NextTabOrBuffer(1)<CR>
nnoremap gT :call NextTabOrBuffer(-1)<CR>
" --- --- Plugin keybinds
nnoremap \ :NERDTreeToggle<CR>
nnoremap <F11> :Goyo<CR>
nnoremap <F5> :GundoToggle<CR>
" --- }}}
" }}}
" Part V: Can I has() options? {{{
if has('gui_running') " --- GVIM {{{
    " --- Common GUI options
    set guioptions-=m  " remove menu bar
    set guioptions-=T  " remove toolbar
    set guioptions-=r  " remove right-hand scroll
    set guioptions-=L  " remove left-hand scroll
    set guioptions-=e  " remove GUI tabline; use consoley one instead
    "set lines=25       " window is 25 lines high
    "set columns=80     " window is 80 columns long
    " --- Set fonts for different systems
    if has("gui_gtk2")
        set guifont=Inconsolata\ for\ Powerline,Inconsolata
        let g:airline_powerline_fonts = 1
    elseif has("x11") " also works with GTK 1
        "set guifont=*-lucidatypewriter-medium-r-normal-*-*-180-*-*-m-*-*
    elseif has("gui_win32")
        "set guifont=Inconsolata_for_Powerline:h15
        set guifont=Consolas:h11:cANSI
        " Consolas don't have powerline arrows
        let g:airline_powerline_fonts = 0
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
    let g:ctrlp_cmd = 'CtrlP D:\Dropbox\'
    let g:ctrlp_cache_dir = 'D:\Dropbox\apps\ctrlp'
    " Don't use NERDTree. Don't work in Windows right.
    nnoremap \ :CtrlP D:\Dropbox\<CR>
    " No python support :(
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
