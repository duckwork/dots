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

set history=1000                  " set history length

" What's saved between sessions in a viminfo file
set viminfo='100                  " Save marks for 100 files max
set vi    ^=!                     " Save global all-caps variables
set vi    +=<50                   " Save 50 lines of registers
set vi    +=s10                   " Save only first 10 Kb of each register
set vi    +=h                     " Disable 'hlsearch' on saved files

set laststatus=2                  " Always show statusline
" set statusline=%!StatusLine()     "   [see function below]
set showtabline=2                 " Always show tabline
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

set directory=$HOME/.vim/swap//   " directory for swap files
set backupdir=$HOME/.vim/backup// " directory for backups
set undodir=$HOME/.vim/undoes//   " directory for UNDO TREE
set backupcopy=yes                " copy the original and overwrite
set backup                        " Keep backup files, just in case
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
autocmd BufEnter * silent! lcd %:p:h
autocmd ColorScheme * call UpdateCursorLine()

augroup TextEditing
    au!
    au BufNewFile,BufRead *.md set ft=markdown spell
    au FileTYpe vimwiki,markdown,text setlocal spell
augroup END

augroup ReloadVimrc
    au!
    au BufWritePost $MYVIMRC source $MYVIMRC
augroup END

augroup CursorLine
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

augroup TrailingSpaces
    au!
    au InsertEnter * match none '\s\+$'
    au InsertLeave * match Error '\s\+$'
augroup END

" augroup StatusUpdate
"     au!
"     au VimEnter,WinEnter,BufWinEnter,BufUnload * call SetStatusLine()
"     au BufWritePost $MYVIMRC call SetStatus()
" augroup END

augroup ft_Help
    au!
    au FileType help setlocal nospell
    au BufWinEnter *.txt
                \ if &ft == 'help' && winwidth(0) >=2 * g:tw |
                \     wincmd L |
                \ endif
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
        else
            return printf('%2d%%', a:nr)
        endif
    endfunction "
    let r.myperc = s:myperc(r.perc)

    return r
endfunction "}}}
function! RenameFile() " {{{
    let old_name = expand('%')
    let new_name = input('New file name:', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        " only work on Linux?
        exec ':silent !rm ' . old_name
        redraw!
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
" function! StatusLine(winnr) " {{{
"     let status = ''
"     let isactive = winnr() == a:winnr
"     let buffer = winbufnr(a:winnr)

"     let ismodified = getbufvar(buffer, '&modified')
"     let isreadonly = getbufvar(buffer, '&readonly')
"     let filename = bufname(buffer)

"     function! Color(isactive, group, content)
"         if a:isactive && !has('win32')
"             return '%' . a:group . '*' . a:content . '%*'
"         else
"             return a:content
"         endif
"     endfunction

"     " Left side ===================================================
"     " Column indicator
"     let status .= '%1*' . (col('.') / 100 >= 1 ? '%v ' : ' %2v ') . '%*'

"     " Filename
"     let status .= ' ' . Color(isactive, 4, isactive ? '»' : '›')
"     let status .= ' %<'

"     if filename == '__Gundo__'
"         let status .= 'Gundo'
"     elseif filename == '__Gundo_Preview__'
"         let status .= 'Gundo Preview'
"     elseif filename == ''
"         let status .= '______'
"     else
"         let status .= '%f'
"     endif

"     let status .= ' ' . Color(isactive, 4, isactive ? '«' : '')

"     " File status indicators
"     let status .= Color(isactive, 2, ismodified ? ' + ' : '')
"     let status .= Color(isactive, 2, isreadonly ?
"                 \ &ft == 'help' ? ' ? ' : ' ‼ '
"                 \ : '')
"     if isactive && &paste
"         let status .= '%2*' . ' P ' . '%*'
"     endif

"     let status .= '%=' " Gutter ====================================

"     " Right side ===================================================
"     let status .= '%p%%'

"     return status
" endfunction " }}}
" function! SetStatusLine() " {{{
"     for nr in range(1, winnr('$'))
"         call setwinvar(nr, '&statusline', '%!Status('.nr.')')
"     endfor
" endfunction " }}}
" function! TabLine() " {{{
" endfunction " }}}
" Custom theming
function! UpdateCursorLine() " {{{
    if &bg == 'dark'
        hi CursorLineNr guifg=#6c71c4 gui=bold
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
Plugin 'chrisbra/NrrwRgn'             " Open region in new win to edit

" COLORS & EYECANDY
Plugin 'junegunn/seoul256.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'itchyny/lightline.vim'
" Plugin 'bling/vim-airline'            " a better statusline

" FORMATTING TEXT
Plugin 'godlygeek/tabular'            " Easy aligning of text
Plugin 'junegunn/vim-easy-align'      " Vim alignment plugin
Plugin 'Lokaltog/vim-easymotion'      " No more counting objects
Plugin 'osyo-manga/vim-over'          " Preview :s/ searches
" [ by TIM POPE ]
Plugin 'tpope/vim-abolish'            " Enhanced search and replace
Plugin 'tpope/vim-commentary'         " Easier commmenting
Plugin 'tpope/vim-repeat'             " Repeat plugin commands with .
Plugin 'tpope/vim-speeddating'        " <C-a>,<C-x> on dates and times
Plugin 'tpope/vim-surround'           " Format surroundings easily

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
if has('python')
    Plugin 'vim-scripts/gundo'        " Visualize Vim's undo tree
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
" let g:ctrlp_status_func = {
"           \ 'main': 'CtrlPStatusLine',
"           \ 'prog': 'CtrlPProgressLine',
"           \ }
let g:ctrlp_extensions = [
            \ 'dir',
            \ 'line',
            \ 'mixed',
            \ ] " Search for dirs to :cd, lines in open bufs,
                " files, buffers, MRU at once
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
let g:lightline = {
            \ 'colorscheme': 'solarized'
            \ }
" Solarized
let g:solarized_menu = 0
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

nnoremap <F5> :GundoToggle<CR>

vnoremap \| :Tabularize /

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
function! CtrlPStatusLine() " {{{
endfunction " }}}
function! CtrlPProgressLine() " {{{
endfunction " }}}
" }}}
" Plugin autocommands {{{
augroup GoyoEvents
    au!
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
endif

let g:ctrlp_mruf_case_sensitive = has('win32') ? 0 : 1
" }}}
"}}}

colorscheme solarized
