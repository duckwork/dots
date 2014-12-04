" ViMrc ReVised
" Case Duckworth (mahatman2)
" vim:tw=0:nowrap:nolinebreak

set nocompatible                    " be iMproved

" SETTINGS {{{
syntax on                           " syntax highlighting
set synmaxcol=300                   " stop syntax coloring at 300 col

set number                          " show line numbers in gutter
set relativenumber                  " show line nums relative to current

set autoread                        " reload on a change, automagically
set lazyredraw                      " don't redraw macros til done
set hidden                          " Don't close unused buffers

set noerrorbells                    " don't beep on errors
set visualbell                      " flash instead of beeping

set splitbelow                      " new splits appear below current window
set splitright                      " new splits appear to right of current

set formatoptions-=ro               " disable autocomments in Insert mode
set backspace=indent,eol,start      " backspace across these things

" Which keys move between lines
set whichwrap=b,s                   " <BS> and <Space>
set ww      +=<,>                   " Arrow keys (N, V)
set ww      +=[,]                   " Arrow keys (I, R)

set encoding=utf-8                  " encoding = utf-8.
set fileencoding=utf-8              " because year = 2014.
set fileformat=unix                 " Unix file ending default
set fileformats=unix,dos            " but recognize DOS line endings

set spelllang=en_us                 " I write in English
set spellfile=~/.vim/spell/en.utf-8.add

set history=1000                    " set history length

" What's saved between sessions
set viminfo='100                    " Save marks for 100 files max
set vi    ^=!                       " Save global all-caps variables
set vi    +=<50                     " Save 50 lines of registers
set vi    +=s10                     " Save only first 10 Kb of each register
set vi    +=h                       " Disable 'hlsearch' on saved files

set laststatus=2                    " Always show statusline
" set showtabline=2                 " Always show tabline
" set tabline=%!TabLine()           " [see function below]
set wildmenu                        " Enhanced command-line completion
set ruler                           " Show where the cursor is in the file
set showcmd                         " Show command as it's being typed

set shortmess=a                     " same as shm=filmnrwx
set shm     +=oO                    " overwrite file msgs
set shm     +=tT                    " truncate msgs in [c] mode
set shm     +=I                     " don't give intro msg on vim start

set foldenable                      " Enable folding
set foldmethod=marker               " {{{ }}} mark folds
" set foldlevel=2                     " Start open to second level
set foldcolumn=0                    " Fold columns in gutter
set foldtext=FoldLine()             " Define what folded folds look like

set scrolloff=8                     " Keep lines between cursor and edges
set sidescrolloff=1                 " Keep columns between cursor and edges
set sidescroll=1                    " Scroll one column at a time sideways

let g:tw = 78                       " Set textwidth without really setting tw
" let &textwidth = g:tw             " Set textwidth
set wrap                            " Soft wrap lines to window
if has('linebreak')
    set linebreak                   " Wrap at words (def by 'breakat')
    set breakindent                 " soft-wrapped lines indent to prev line
endif
set list                            " show some non-printing characters
set listchars=tab:»»                " Show these non-printing characters
set lcs     +=trail:·               " TODO: think about indenting chars
set lcs     +=nbsp:~                "    (spaces or tabs?)
let &showbreak = '└ '               " when a line wraps, show it with this
match Error /\s\+$/                 " match trailing whitespace to error

set nostartofline                   " stay in column with gg, G, etc.
set virtualedit=insert              " allow cursor past EOL in Insert mode
set ve        +=block               " allow cursor past EOL in V-block

let &colorcolumn = g:tw             " highlight this column
set cursorline                      " highlight the line being edited

set expandtab                       " use spaces instead of tabs
set autoindent                      " copy indent when inserting a new line
set smartindent                     " autoindent based on conditions
set shiftwidth=4                    " number of spaces for (auto)indent
set softtabstop=4                   " number of spaces a <Tab> equals
set shiftround                      " indent to nearest tabstop

set incsearch                       " find the next match as we type
set hlsearch                        " highlight search matches
set ignorecase                      " ignore case as we search
set smartcase                       " ... unless a capital appears

set gdefault                        " default to global (line) substitutions
set magic                           " use better regexp

set autowriteall                    " autowrite on many commands

set directory=$HOME/.vim/swap//     " directory for swap files
set backupdir=$HOME/.vim/backup//   " directory for backups
set backupcopy=yes                  " copy the original and overwrite
set backup                          " Keep backup files, just in case

set viewdir=$HOME/.vim/views//      " Save fileviews
set viewoptions=folds               " Include folds in fileviews
set vop       +=cursor              " Include where the cursor was
set vop       +=slash               " backslashes in fnames = forward
set vop       +=unix                " Unix EOL fmt even when on Windows
" set vop       +=options             " Include local options and mappings

set confirm                         " Confirm before quit, instead of error

set undolevels=10000                " Max changes that can be undone
"}}}
" KEYMAPS {{{

" Easier mode changing
noremap ; :
inoremap jj <Esc>
inoremap kk <Esc>
" Better leader
let mapleader = ','

" Better movement
nnoremap <expr> j v:count > 0 ? 'j' : 'gj'
nnoremap <expr> k v:count > 0 ? 'k' : 'gk'
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
" Make S opposite of D (S = cc by default, so useless!)
nnoremap S d0

" <BS> backspaces in normal mode
nnoremap <BS> X
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

" Edit and source rc easily
" nnoremap <leader>er :edit $MYVIMRC<CR>
nnoremap <leader>er :call Concrastinate('edit $MYVIMRC',1030,1700)<CR>
nnoremap <leader>re :source $MYVIMRC<CR>

" Change current directory to filepath
nnoremap <leader>cd :cd %:p:h<CR>
" Explore the current directory
nnoremap <silent> - :Explore<CR>
" List open buffers and switch to one
nnoremap gb :ls<CR>:b<Space>

" Remove search highlight
nnoremap <silent> <leader>/ :nohlsearch<CR>
" Remove end-of-line whitespace
nnoremap <silent> <leader>rs mz:%s/\s\+$//<CR>:let @/=''<CR>`z
" Remove blank lines
nnoremap <silent> <leader>rb mz:g/^$/d<CR>:let @/=''<CR>`z
" Toggle background
nnoremap <silent> <leader>bg :call ToggleBG()<CR>

" Close buffer, or if last buf, quit vim
nnoremap <F12> :call CloseBufWin()<CR>
" Navigate to previous-focused buffer
nnoremap <silent> Q :b#<CR>
" Move to next buffer if there's only one tab
nnoremap gt :<C-U>call ChTabBuf(v:count1)<CR>
nnoremap gT :<C-U>call ChTabBuf(-v:count1)<CR>

" Linux only: because `sudo vim` is easy to forget
cmap w!! %!sudo tee > /dev/null %
"}}}
" AUTOCOMMANDS {{{
augroup Filestuff "{{{
    au!
    " Autosave files
    au FocusLost * silent! wall
    " Auto-remove whitespace when saving
    au BufWritePre * %s/\s\+$//e
    " Auto-source vimrc on save
    "au BufWritePost $MYVIMRC source $MYVIMRC

    " Auto chdir
    au BufEnter * if &ft != 'help'
                \ | silent! lcd %:p:h | endif
    " Jump to previous position in file
    au BufReadPost * normal `"
augroup END "}}}
augroup Windowstuff "{{{
    au!
    " Change cursorline to something okay
    " autocmd ColorScheme * call UpdateCursorLineNumber()

    " Update statusline
    au VimEnter,WinEnter,BufWinEnter * call <SID>RefreshStatus()

    " Remove annoying shit when in insert mode or outside window
    au WinLeave,InsertEnter,BufWinLeave * call ListPlus('off')
    au WinEnter,InsertLeave,BufWinEnter * call ListPlus('on')

    " Save and restore windowviews
    au BufWinLeave * silent! mkview
    au BufWinEnter * silent! loadview
augroup END "}}}
" Filetypes {{{
augroup ft_Help
    au!
    au FileType help setlocal nospell
    " Move help window to right if wide enough
    au BufWinEnter *.txt
                \ if &ft == 'help' && winwidth(0) >=2 * g:tw |
                \     wincmd L |
                \ endif
    au FileType help nnoremap <buffer> <CR> <C-]>
    au FileType help nnoremap <buffer> <BS> <C-t>
augroup END

augroup ft_Text
    au!
    au BufNewFile,BufRead *.txt setf pandoc
    au FileType *wiki,markdown,pandoc setlocal spell
    " au FileType markdown call Typewriter('on')
augroup END

" augroup Status_ft
"     au!
"     au FileType text,markdown,pandoc    let w:ftpart = '“ %%<%s ”' |
"                 \                       let w:ftshow = 0
"     au FileType html,css,javascript,php let w:ftpart = '< %%<%s >' |
"                 \                       let w:ftshow = 0
"     au FileType *wiki                   let w:ftpart = '= %%<%s =' |
"                 \                       let w:ftshow = 0
"     au FileType help                    let w:ftpart = '| %%<%s |' |
"                 \                       let w:ftshow = 0
"     au FileType *sh                     let w:ftpart = '[ %%<%s ]' |
"                 \                       let w:ftshow = 0
" augroup END
" }}}
"}}}
" CAN I HAS(?) {{{
if has('win32') " {{{
    let &runtimepath .= ',$HOME\.vim'
    let &clipboard = has('unnamedplus') ? 'unnamedplus' : 'unnamed'
    set viminfo+=rA:,rB:

    nnoremap <silent> <F10> :simalt ~n<CR>

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
    set go        +=c " don't use pop-up boxes

    " Set colors to 256
    set t_Co=256

    " Change font based on system
    if has('gui_gtk2')
        set guifont=Inconsolata
    elseif has('x11')
        "set guifont=*-lucidatypewriter-medium-r-normal-*-*-180-*-*-m-*-*
    elseif has('gui_win32')
        set guifont=Consolas:h11:cANSI
        " set guifont=Fantasque_Sans_Mono:h11:cANSI
        " set guifont=PT_Mono:h11:cANSI
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
" }}}
"}}}
" FUNCTIONS {{{
" Tools
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
function! Concrastinate(cmd, ...) " {{{
    " TODO: work with a file list
    " keep from vimrc hacking during work
    " timestart, timeend must be fmt %H%M
    " cmd is a exe command
    let current_time = strftime("%H%M")
    let s:timestart = exists("a:1") ? a:1 : 0800
    let s:timeend  = exists("a:2") ? a:2 : 1800

    if current_time >= s:timestart && current_time <= s:timeend
        echohl WarningMsg
        echom "Sorry, can't " a:cmd ", it's b/w "
                    \ s:timestart " and " s:timeend "."
        echohl None
    else
        " TODO: fnameescape() it
        " TODO: add option to add TODO: to end of file
        exe a:cmd
    endif
endfunction " }}}
" Managing buffers, tabs, windows
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
" Custom interface lines
function! FoldLine() " {{{
    let line = getline(v:foldstart)
    let foldedlinecount = printf('%3d', v:foldend - v:foldstart)
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
    return '> ' . v:foldlevel . ': ' . foldedlinecount . ' ' . line . ' <'
endfunction " }}}
function! StatusLine(winnr) " {{{
    let status     = ''

    let buffer     = winbufnr(a:winnr)
    let fname      = bufname(buffer)
    let ftype      = getbufvar(buffer, '&filetype')

    let isactive   = winnr() == a:winnr
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
        let status .= '%#CursorLine#'.'>%n. '
    endif

    " ‼‹›℗←↑→↓◊∫∂↔↕Ω˂˃˄˅§«»±¶¤Ππ‘’“”⌂
    " let bufcomment = getbufvar(buffer, '&commentstring')
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
    " Filename
    if fname =~ '__Gundo' || ftype =~ 'netrw' || fname == ''
        let fname  = len(ftype) > 0 ? ftype : '______'
        let showft = 0
    " elseif fname == ''
    "     let fname  = '______'
    "     let showft = 0
    else
        let fname  = '%f'
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
    if showft || winwidth(0) > g:tw + &fdc + &nu * &nuw + 4
        let status .= '[' . ftype . ']'
    else
        let status .= ''
    endif
    if ftype =~? 'text' || ftype =~? 'm.*d.*' || ftype ==? 'pandoc'
                \|| ftype =~? 'wiki'
        let status .= ' %{WordCount()}'
    endif
    let status .= '%#StatusLine#'
    let status .= Color(!isactive, 'CursorLine', ' %3p%% ')

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
" function! UpdateCursorLineNumber() " {{{
"     if &bg == 'dark'
"         hi CursorLineNr guifg=#6c71c4 gui=bold
"     endif
" endfunction " }}}
function! Typewriter(switch) " {{{
    function! s:typewriter_on()
        let s:wrap = &wrap
        let s:linebreak = &linebreak
        let s:tw = &textwidth
        let s:list = &list
        let s:cursorline = &cursorline
        let s:cursorcolumn = &cursorcolumn

        setlocal wrap
        setlocal linebreak
        let &l:textwidth = get(g:, 'tw', 78)
        setlocal nolist
        setlocal nocursorline
        setlocal nocursorcolumn

        return 1
    endfunction

    function! s:typewriter_off()
        let &l:wrap = get(s:, 'wrap', &wrap)
        let &l:linebreak = get(s:, 'linebreak', &linebreak)
        let &l:textwidth = get(s:, 'textwidth', &textwidth)
        let &l:list = get(s:, 'list', &list)
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
function! ToggleBG() "{{{
    if &bg == 'light'
        set background=dark
    else
        set background=light
    endif
endfunction "}}}
function! ListPlus(switch) "{{{
    function! s:listplus_off()
        let s:cul  = &cursorline
        let s:cc   = &colorcolumn
        let s:list = &list
        let s:rnu  = &relativenumber

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

    if getbufvar("%", "&ft") !~? 'help'
        if a:switch ==? 'on'
            let b:listplus_enabled = <SID>listplus_on()
        elseif a:switch ==? 'off'
            let b:listplus_enabled = <SID>listplus_off()
        elseif a:switch =~? 'tog'
            let b:listplus_enabled = get(b: 'listplus_enabled', 0)
                        \ ? <SID>listplus_off()
                        \ : <SID>listplus_on()
        endif
    endif
endfunction "}}}
function! DoCmds(...) " {{{
    for cmd in a:000
        if exists(":".cmd) == 2 " full match with command
            exe "normal! ".cmd
        endif
    endfor
endfunction " }}}
" }}}
" PLUGINS {{{
" Vim-plug {{{
call plug#begin('~/.vim/plugged')

" GUI
" Plug 'duckwork/vim-buftabline'          " show vim buffers in tabline
Plug 'talek/obvious-resize',            " Resive ViM windows obviously
            \ { 'on': [ 'ObviousResizeUp',
            \           'ObviousResizeLeft',
            \           'ObviousResizeRight',
            \           'ObviousResizeDown',
            \         ] }
" Colors
Plug 'duckwork/vim-colors-pencil'
Plug 'altercation/vim-colors-solarized'
Plug 'zenorocha/dracula-theme', { 'rtp': 'vim' }
Plug 'Suave/vim-colors-guardian'

" WRITING
" Prose
Plug 'junegunn/goyo.vim',               " distraction-free writing
            \ { 'on': 'Goyo' }
Plug 'duckwork/limelight.vim',          " highlight current para
            \ { 'on': 'Limelight' }

" Code
Plug 'tpope/vim-commentary'             " Easier commmenting
Plug 'tpope/vim-endwise'                " Auto-add 'end*'s in code
Plug 'tpope/vim-characterize'           " Modernize `ga` behavior

" NAVIGATING FILESYSTEM
Plug 'kien/ctrlp.vim'                   " A fuzzy file finder
Plug 'dockyard/vim-easydir'             " Create new dirs on-the-fly
Plug 'tpope/vim-vinegar'                " Better netrw integration
Plug 'xolox/vim-shell'                  " Integrate ViM and environment
Plug 'xolox/vim-misc'                   " Required by vim-shell

" EXTENDING VIM OPERATIONS
Plug 'tpope/vim-repeat'                 " Repeat plugin commands with .
" Search & Replace
Plug 'nelstrom/vim-visual-star-search'  " Use * or # from V-Block
Plug 'tpope/vim-abolish'                " Enhanced search and replace
" Formatting
Plug 'godlygeek/tabular',               " Easy aligning of text
            \ {'on': 'Tabular' }
" Plug 'junegunn/vim-easy-align'          " Vim alignment plugin
Plug 'AndrewRadev/splitjoin.vim'        " Easily split and join code
Plug 'tpope/vim-speeddating'            " <C-a>,<C-x> on dates and times
Plug 'tommcdo/vim-exchange'             " Easy text exchange operator
" Textobjects
Plug 'Lokaltog/vim-easymotion'          " No more counting objects
Plug 'wellle/targets.vim'               " Lots of new textobjects
Plug 'michaeljsmith/vim-indent-object'  " a textobj for indentblocks
Plug 'tpope/vim-surround'               " Format surroundings easily

" FILETYPES
Plug 'mattn/emmet-vim',                 " Zencoding for HTML
            \ { 'for': [ 'html', 'xml', ] }
Plug 'gregsexton/MatchTag'              " Match HTML tags with %

Plug 'vim-pandoc/vim-pandoc'            " Pandoc helpers
Plug 'reedes/vim-litecorrect'           " autocorrect w/customization

Plug 'vimwiki/vimwiki'                  " Personal wiki with ViM
Plug 'freitass/todo.txt-vim'            " Syntax + keybinds for todo.txt

Plug 'sheerun/vim-polyglot'             " Many syntax defs
Plug 'hail2u/vim-css3-syntax'           " syntax file for CSS3
Plug 'dogrover/vim-pentadactyl'         " ftdetect, ftplugin, syntax
Plug 'vim-pandoc/vim-pandoc-syntax'     " Pandoc syntax

Plug 'vim-scripts/matchit.zip'          " Better matchit plugin (newer ver)

" PLUGINS THAT REQUIRE THINGS
if executable('git')
    Plug 'tpope/vim-fugitive'           " Git integration
    Plug 'airblade/vim-gitgutter'       " Git stuff in signs column
endif
if executable('ag')
    Plug 'rking/ag.vim'                 " Ag implementation
endif
if executable('diff')
    Plug 'mbbill/undotree'              " Visualize Vim's undo tree
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g "" '
    nnoremap <F5> :UndotreeToggle<CR>
elseif has('python')
    Plug 'vim-scripts/gundo'            " Visualize Vim's undo tree
    nnoremap <F5> :GundoToggle<CR>
endif

call plug#end()                         " req'd
"}}}
" Plugin settings {{{
" Ag
let g:agprg = 'ag --column --smart-case'
let g:aghighlight = 1 " highlight searches
" Buftabline
let g:buftabline_show = 1 " only show if at least 2 buffers
let g:buftabline_numbers = 1 " show buffer numbers in list
let g:buftabline_indicators = 1 " show if buffers are modified
" Colorscheme Pencil
let g:pencil_spell_undercurl = 1
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
" Pandoc
let g:pandoc#modules#disabled = [ 'menu' ] " Get rid of Pandoc menu
let g:pandoc#command#custom_open = "PandocOpen" " function defined below
let g:pandoc#filetypes#handled = [ 'markdown', 'rst', 'textile', ]
let g:pandoc#folding#fdc = &fdc
let g:pandoc#formatting#mode = 'hA' " hard wrap, autoformat smart
let g:pandoc#formatting#textwidth = g:tw
let g:pandoc#keyboard#sections#header_style = 's' " enable setext for h1,2
let g:pandoc#spell#default_langs = ['en']
let g:pandoc#toc#position = "left" " Table of contents
let g:pandoc#toc#close_after_navigating = 0 " <CR> navs, <C-CR> navs + closes
" Pandoc syntax
let g:pandoc#syntax#conceal#blacklist = [
            \ 'titleblock',
            \ 'definition',
            \ 'list',
            \ 'ellipses',
            \ ]
" Solarized
let g:solarized_menu = 0
" Splitjoin
let g:splitjoin_split_mapping = 'gK'
" Undotree
let g:undotree_WindowLayout = 2
let g:undotree_DiffAutoOpen = 1
let g:undotree_SetFocusWhenToggle = 1
" Vim-Plug
" let g:plug_url_format = 'https://github.com/%s.git'
" Vim-shell
let g:shell_mappings_enabled   = 0 " disable default mappings
let g:shell_fullscreen_message = 0 " don't help me to get out of fullscreen
" Vimwiki
let g:vimwiki_hl_headers = 0 " enable different colored headers
let g:vimwiki_hl_cb_checked = 1 " hilight [X] with Comment
" }}}
" Plugin keymaps {{{
let maplocalleader = ',' " same as leader for now.

nnoremap <F11> :Goyo<CR>
nnoremap <S-F11> :Fullscreen<CR>

vnoremap \| :Tabularize /

nnoremap <C-o> :CtrlPMRU<CR>
" see non-plugin mapping gs :s/
nnoremap gS :%S/
vnoremap gS :S/
" J/K intelligently SplitJoin.vim or fallback to default
nnoremap <silent> J :<C-u>call <SID>try('SplitjoinJoin', 'J')<CR>
nnoremap <silent> K :<C-u>call <SID>try('SplitjoinSplit', "i\r")<CR>

" remap motion maps !
if exists('g:EasyMotion_loaded')
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
endif

" Disable pandoc#formatting#autoformat
" TODO: toggle?
nnoremap <F3> :call pandoc#formatting#DisableAutoformat()<CR>

" Window resizing with ObviousResize
nnoremap <C-UP>    :<C-u>call <SID>try('ObviousResizeUp', 'wincmd +')<CR>
nnoremap <C-DOWN>  :<C-u>call <SID>try('ObviousResizeDown', 'wincmd -')<CR>
nnoremap <C-LEFT>  :<C-u>call <SID>try('ObviousResizeLeft', 'wincmd <')<CR>
nnoremap <C-RIGHT> :<C-u>call <SID>try('ObviousResizeRight', 'wincmd >')<CR>
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
" }}}
" Plugin autocommands {{{
augroup GoyoEvents
    au!
    au User GoyoEnter Limelight
    au User GoyoEnter set nocursorline
    au User GoyoEnter call <SID>RefreshStatus(1)
    au User GoyoEnter call setwinvar(winnr(), '&statusline',
                       \ '%=%#Comment# [ %{WordCount()} ] ')

    au User GoyoLeave Limelight!
    au User GoyoLeave set cursorline
    au User GoyoLeave call <SID>RefreshStatus()
augroup END
augroup Litecorrect
    au!
    au FileType markdown,mkd call litecorrect#init()
    au FileType textile call litecorrect#init()
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

" colorscheme solarized
colorscheme pencil
