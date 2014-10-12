" _______________ VIMRC -- CASE DUCKWORTH _________________
" vim:foldlevel=0:textwidth=0:nowrap:nolinebreak
set nocompatible
" Part 0: Vundle {{{
filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()
"if exists(":Plugin")
    Plugin 'gmarik/Vundle.vim'         " let Vundle manage Vundle, req'd
    " Plugins
    " --- Using ViM {{{
    if has("gui_running")
        Plugin 'Rykka/clickable.vim'   " Folding, links, files clickable
    endif
    Plugin 'Lokaltog/vim-easymotion'   " No more counting objects
    Plugin 'chrisbra/NrrwRgn'          " Open region in new window, edit, reinsert
    Plugin 'vim-scripts/gundo'         " Visualize Vim's undo tree (not on Windows?)
    " --- --- VIM eyecandy
    Plugin 'bling/vim-airline'         " a better statusline
    Plugin 'junegunn/goyo.vim'         " distraction-free writing like Writeroom
    " --- --- Colorschemes
    "Plugin 'flazz/vim-colorschemes'    " huge collection of colorschemes
    Plugin 'reedes/vim-colors-pencil'  " pencil colorscheme
    Plugin 'Pychimp/vim-luna'          " luna colorscheme
    Plugin 'Pychimp/vim-sol'           " lighter version of luna
    " --- --- Vim Wiki
    Plugin 'vimwiki/vimwiki'
    " --- }}}
    " --- Editing Files {{{
    " --- --- Navigating and saving
    Plugin 'scrooloose/nerdtree'       " NERDTree - an easy-to-use file manager
    Plugin 'kien/ctrlp.vim'            " a fuzzy finder
    "Plugin 'mhinz/vim-startify'        " start page with recent files, etc.
    Plugin 'dockyard/vim-easydir'      " Create new dirs on-the-fly when saving
    " --- --- Working within files
    Plugin 'scrooloose/nerdcommenter'  " toggle comments easily
    Plugin 'tpope/vim-surround'        " Surround text objects with things
    Plugin 'godlygeek/tabular'         " easy formatting of text tables
    Plugin 'ervandew/supertab'         " perform all complet. with Tab (I)
    Plugin 'AndrewRadev/splitjoin.vim' " Split and join code easily
    " --- --- Filetypes
    " --- --- --- HTML
    Plugin 'mattn/emmet-vim'           " Zencoding for HTML
    Plugin 'gregsexton/MatchTag'       " Match HTML tags like parentheses
    " --- --- --- CSS
    "Plugin 'hail2u/vim-css3-syntax'    " syntax file for CSS3
    " --- --- --- Markdown " These should be handled by Pandoc, below
    "Plugin 'tpope/vim-markdown'
    "Plugin 'nelstrom/vim-markdown-folding' " fold Markdown on headings
    " --- --- --- Pandoc
    Plugin 'vim-pandoc/vim-pandoc'
    Plugin 'vim-pandoc/vim-pandoc-syntax'
    " --- --- --- Polyglot: over fifty languages' syntax files
    Plugin 'sheerun/vim-polyglot'
    " --- }}}
"endif
call vundle#end()                      "req'd
filetype plugin indent on              "req'd
"}}}
" Part I: Usability Configuration {{{
" because vanilla vim, though great, is still lacking.
" --- Display
syntax on                      " syntax highlighting is great
set number                     " and line numbers, too
set autoread                   " reload on a change, automagically
set lazyredraw                 " don't redraw everything (faster macros, etc.)
set hidden                     " Don't close unused buffers
" --- Using Vim
set formatoptions-=r           " disable autocomments when hitting ENTER (I)
set formatoptions-=o           " disable autocomments when hitting O or o (I)
set history=1000               " set history of commands to 1000 long.
runtime macros/matchit.vim     " enable matchit plugin (better %)
set backspace=indent,eol,start " backspace across autoindents, lines, insertstart
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
" Part II: Customization {{{
" --- Appearance {{{

set background=dark    " Dark background (duh)
if has("gui_running") || &t_Co>=88
    colorscheme pencil     " inspired by iA Writer
    set colorcolumn=80     " highlight column 80
    set cursorline         " highlight the line the cursor's on
else                       " 8-color terms can't handle colors
    colorscheme desert
    set colorcolumn=
    set nocursorline
endif

set laststatus=2       " use status line, always.

" Statusline
set statusline=%t          " basename of file
set statusline+=%H         " help buffer flag ,HLP
set statusline+=%R         " read-only flag ,RO
set statusline+=\ %m       " modified flag [+]
set statusline+=%=         " begin right-align
set statusline+=%y\        " file type
set statusline+=%3p%%      " scroll percentage
set statusline+=\ -%3l/%3L " current line / total lines
set statusline+=\|%2c      " current column

set wildmenu           " tab completion with a menu
set ruler              " show ruler
set showcmd            " Show partial commands as-you-type

match ErrorMsg '\s\+$' " Show trailing whitespace as error

set scrolloff=8        " keep lines at bottom and top when scrolling
set sidescrolloff=4    " keep lines at the left and write when scrolling
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
set smartcase         " ... unless a capital appears (it's probs meant)

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
nnoremap <leader>h K

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

" I (should) also edit life.txt a lot. SO:
"let g:lifefile = 'D:\Dropbox\life.txt'  change this as necessary
"nnoremap <leader>el :edit `=g:lifefile`<CR>

" use <Space> to remove search highlight
nnoremap <leader><Space> :nohlsearch<return><Esc>
" Remove whitespace from the ends of lines
nnoremap <leader>r<Space> :%s/\s\+$//e<CR>
" --- }}}
" }}}
" Part III: Custom functions {{{
function! WordCount() " TODO: Generalize to count words, characters, etc.
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
    return s:word_count . "w"
endfunction
nnoremap <leader>wc :echo 'words: '.WordCount()<CR>

function! ToggleBackground()
    if &background=="dark"
        set background=light
    else
        set background=dark
    endif
endfunction
nnoremap <F6> :call ToggleBackground()<CR>

" TODO: a function that switches between basename and full path in stl
" }}}
" Part IV: Plugin Config {{{

" Toggle file browser
nnoremap \ :NERDTreeToggle<CR>

" map emmet (ZenCoding) to <C-E>
let g:user_emmet_leader_key = '<c-e>'

" Ctrl-P settings
let g:ctrlp_max_depth = 100         " max depth of search
let g:ctrlp_max_files = 0           " no limit to how many files
let g:ctrlp_use_caching = 1         " enable caching
let g:ctrlp_clear_cache_on_exit = 0 " enable cross-session caching
let g:ctrlp_cache_dir = $HOME.'/.cache/ctrlp'

" AIRLINE OPTIONS
"if exists(":AirlineRefresh") " Is Airline installed?
    " --- The right section is busy.
    let g:airline_section_z = '%3p%%|%3l:%2c'
    let g:airline_section_y = "%{WordCount()}"
    " --- include character count
    "let g:airline_section_z = '%3p%%|%4l:%2c|%{strwidth(join(getline(1,"$")))}c'
    let g:airline_powerline_fonts = 1
    "AirlineRefresh
    set noshowmode         " Airline already shows mode, not necessary
"endif

" Markdown-folding options
let g:markdown_fold_style = 'nested'

" Goyo on <F11> like fullscreen
nnoremap <F11> <Esc>:Goyo<CR>
let g:goyo_width = 70
let g:goyo_margin_top = 4
let g:goyo_margin_bottom = 4

" Toggle UNDO tree view
nnoremap <F5> :GundoToggle<CR>
let g:gundo_preview_bottom = 1 " Preview takes up full width
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
    let g:gundo_disable = 1
    let g:pandoc#modules#disabled = ["bibliographies"]
    " Goyo maximizes window
    augroup win_Goyo_events         " Call these functions
        autocmd!
        autocmd User GoyoEnter simalt ~x | sleep 10m | Goyo 70
        autocmd User GoyoLeave simalt ~r
    augroup END

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
