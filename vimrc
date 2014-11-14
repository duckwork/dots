" ViMrc ReVised
" Case Duckworth (mahatman2)
" vim:foldenable:foldlevel=0:tw=0:nowrap:nolinebreak

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

" " Make too-long lines obvious, but only on those lines
" call matchadd('ColorColumn', '\%'.g:tw.'v', 100)
let &colorcolumn = g:tw

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
"}}}
" AUTOCOMMANDS {{{
autocmd BufEnter * if &ft != 'help' | silent! lcd %:p:h | endif
autocmd ColorScheme * call UpdateCursorLineNumber()

augroup TextEditing
    au!
    au BufNewFile,BufRead *.{md,markdown,mdown,mkd,mkdn,txt} setf markdown
    au FileType vimwiki,markdown setlocal spell
    au FileType markdown call Typewriter('on')
augroup END

" augroup ReloadVimrc
"     au!
"     au BufWritePost $MYVIMRC source $MYVIMRC
" augroup END

augroup InsertAnnoyances
    au!

    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * if !get(b:, 'typewriter_loaded', 0) |
                \ set cursorline | endif

    au InsertEnter * match none '\s\+$'
    au InsertEnter * set nolist colorcolumn=0
    au InsertLeave * match Error '\s\+$'
    au InsertLeave * if !get(b:, 'typewriter_loaded', 0) |
                \ set list | let &cc = g:tw | endif
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
"}}}
" FUNCTIONS {{{
" --- Tools
function! WordCount() " {{{ I'm only using the WordCount part
    let s:oldstat = v:statusmsg
    let position = getpos('.')
    exe ":silent normal g\<c-g>"
    let status = v:statusmsg

    if status != '--No lines in buffer--' && mode() !~? '[v]'
        return str2nr(split(status)[11]) . 'w'
    else
        return mode() ==? 'v'
                    \ ? split(status)[1] . '/' . split(status)[3] . 'L'
                    \ : split(status)[1] . ':' .
                    \   split(status)[3] . '/' split(status)[5] . 'L'
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

    let buffer = winbufnr(a:winnr)
    let fname  = bufname(buffer)
    let ftype  = getbufvar(buffer, '&filetype')

    let isactive = winnr() == a:winnr
    let ismodified = getbufvar(buffer, '&modified')
    let isreadonly = getbufvar(buffer, '&readonly')
    let ishelp     = ftype == 'help'

    function! Color(isactive, group, content)
        let part = ''
        if a:isactive
            let part .= '%#' . a:group . '#'
            let part .= a:content
            let part .= '%#StatusLine#'
            return part
        else
            return a:content
        endif
    endfunction

    " Left side ===================================================
    " Column indicator
    if isactive
        let status .= '%#CursorLineNr#'.'%3v '.'%#StatusLine#'
    else
        let status .= '%#CursorLine#'.'%2n. '
    endif

    " ‼‹›℗←↑→↓◊∫∂↔↕Ω˂˃˄˅§«»±¶¤Ππ‘’“”⌂
    let bufcomment = getbufvar(buffer, '&commentstring')
    " Filename
    if fname =~ '__Gundo'
        let fname = '∂'
    elseif fname =~ 'NrrwRgn_'
        let fname = substitute(fname, 'NrrwRgn_', '↕ ', '')
    elseif ftype =~ 'netrw'
        let fname = '⌂'
    elseif fname == ''
        let fname = '______'
    else
        let fname = '%f'
    endif
    " Filetype
    if ftype =~? 'text' || ftype =~? 'm.*d.*' || ftype ==? 'pandoc'
        let filepart = '“ %%<%s ”'
        let showft   = 0
    elseif ftype =~? 'htm' || ftype =~? 'css' || ftype =~? 'j.*s.*'
                \ || ftype =~? 'php'
        let filepart = '< %%<%s >'
        let showft   = 0
    elseif ftype =~? 'wiki'
        let filepart = '= %%<%s ='
        let showft   = 0
    elseif ishelp
        let filepart = '%%<%s'
        let showft   = 0
    elseif ftype =~? 'sh$'
        let filepart = '[ %%<%s ]'
        let showft   = 0
    else
        let filepart = '« %%<%s »'
        let showft   = 1
    endif

    if isactive
        let status .= '%#StatusLine# '
        let status .= printf(filepart, fname)
        let status .= ' %#CursorLine#'
    else
        let status .= fname
    endif

    " File status indicators
    let status .= Color(isactive, 'DiffAdd', ismodified ? ' + ' : '')
    let status .= Color(isactive, 'DiffDelete', isreadonly ?
                \ ishelp ? ' ? ' : ' ‼ '
                \ : '')
    if isactive && &paste
        let status .= Color(1, 'DiffChange', ' P ')
    endif

    let status .= '%#CursorLine#'
    let status .= '%=' " Gutter ====================================

    " Right side ===================================================
    let status .= showft ? ftype . ' ' : ''
    if ftype =~? 'text' || ftype =~? 'm.*d.*' || ftype ==? 'pandoc'
                \|| ftype =~? 'wiki'
        let status .= '%{WordCount()} '
    endif
    let status .= '%#StatusLine#'
    let status .= Color(!isactive, 'CursorLine', ' %p%% ')

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
function! TabLine() " {{{
endfunction " }}}
" Custom theming
function! UpdateCursorLineNumber() " {{{
    if &bg == 'dark'
        hi CursorLineNr guifg=#6c71c4 gui=bold
    endif
endfunction " }}}
function! Typewriter(switch) " {{{
    function! s:typewriter_on()
        let s:wrap = &wrap
        let s:linebreak = &linebreak
        let s:tw = &textwidth
        let s:list = &list
        " let s:background = &background
        let s:cursorline = &cursorline
        let s:cursorcolumn = &cursorcolumn

        setlocal wrap
        setlocal linebreak
        let &l:textwidth = get(g:, 'tw', 78)
        setlocal nolist
        " setlocal background=light
        setlocal nocursorline
        setlocal nocursorcolumn

        return 1
    endfunction

    function! s:typewriter_off()
        let &l:wrap = get(s:, 'wrap', &wrap)
        let &l:linebreak = get(s:, 'linebreak', &linebreak)
        let &l:textwidth = get(s:, 'textwidth', &textwidth)
        let &l:list = get(s:, 'list', &list)
        " let &l:background = get(s:, 'background', &background)
        let &l:cursorline = get(s:, 'cursorline', &cursorline)
        let &l:cursorcolumn = get(s:, 'cursorcolumn', &cursorcolumn)

        return 0
    endfunction

    if a:switch ==? 'on'
        let b:typewriter_loaded = <SID>typewriter_on()
    elseif a:switch ==? 'off'
        let b:typewriter_loaded = <SID>typewriter_off()
    elseif a:switch =~? 'tog'
        let b:typewriter_loaded = get(b:, 'typewriter_loaded', 0)
                    \ ? <SID>typewriter_off()
                    \ : <SID>typewriter_on()
    endif
endfunction " }}}
" }}}
" PLUGINS {{{
" Vundle {{{
filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'            " let Vundle manage Vundle

" MAKING WRITING EASIER
Plugin 'junegunn/goyo.vim'            " distraction-free writing
Plugin 'junegunn/limelight.vim'       " highlight current paragraph
" Plugin 'chrisbra/NrrwRgn'             " Open region in new win to edit
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
let g:ctrlp_status_func = {
            \ 'main': 'CtrlPStatusLine',
            \ 'prog': 'CtrlPProgressLine'
            \ }
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

vnoremap \| :Tabularize /

nnoremap <C-o> :CtrlPMRU<CR>

nnoremap gS :%S/
vnoremap gS :S/

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
" }}}
" Plugin autocommands {{{
augroup GoyoEvents
    au!
    au User GoyoEnter Limelight
    au User GoyoEnter set nocursorline
    au User GoyoEnter call <SID>RefreshStatus(1)

    au User GoyoLeave Limelight!
    au User GoyoLeave set cursorline laststatus=2
    au User GoyoLeave call <SID>RefreshStatus()
augroup END
"}}}
" Plugin can I has(?) {{{
" Create directories if not exist
if !isdirectory(expand(g:ctrlp_cache_dir))
    call mkdir(expand(g:ctrlp_cache_dir), 'p')
endif

let g:ctrlp_mruf_case_sensitive = has('win32') ? 0 : 1
" }}}
"}}}

colorscheme solarized
