" ________________________ VIMRC -- CASE DUCKWORTH ___________________________
" vim:foldenable:foldlevel=0:textwidth=0:nowrap:nolinebreak
set nocompatible
" Part 0:   Vundle {{{
filetype off
set runtimepath+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'            " let Vundle manage Vundle

" MAKING EDITING EASIER
Plugin 'junegunn/goyo.vim'            " distraction-free writing
Plugin 'junegunn/limelight.vim'       " highlight only active para
Plugin 'chrisbra/NrrwRgn'             " Open region in new win to ed

" COLORS & EYECANDY
Plugin 'reedes/vim-colors-pencil'
Plugin 'nice/sweater'
Plugin 'altercation/vim-colors-solarized'
Plugin 'sjl/badwolf'

" TIM POPE
Plugin 'tpope/vim-repeat'             " repeat plugin commands with
Plugin 'tpope/vim-surround'           " format surroundings easily
Plugin 'tpope/vim-abolish'            " Enhanced search and replace
Plugin 'tpope/vim-speeddating'        " C-a C-x on dates and times
Plugin 'tpope/vim-commentary'         " commenting

" FORMATTING TEXT
Plugin 'Lokaltog/vim-easymotion'      " No more counting objects
Plugin 'godlygeek/tabular'            " easy formatting of text tabs
Plugin 'reedes/vim-textobj-sentence'  " Improved sentence textobj

" FILESYSTEM
Plugin 'kien/ctrlp.vim'               " a fuzzy finder
Plugin 'dockyard/vim-easydir'         " Create new dirs on-the-fly

" FILETYPES
Plugin 'mattn/emmet-vim'              " Zencoding for HTML
Plugin 'gregsexton/MatchTag'          " Match HTML tags with %
Plugin 'vim-pandoc/vim-pandoc'        " Pandoc helpers
Plugin 'vimwiki/vimwiki'              " Personal wiki with ViM
Plugin 'sheerun/vim-polyglot'         " Many syntax defs
Plugin 'hail2u/vim-css3-syntax'       " syntax file for CSS3
Plugin 'vim-pandoc/vim-pandoc-syntax' " Pandoc syntax

Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-shell'

" PLUGINS THAT REQUIRE X
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
"Plugin 'AndrewRadev/splitjoin.vim' " Split and join code easily
"Plugin 'ervandew/supertab'         " tab completion in (I)

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

let g:shell_mappings_enabled  = 0 " Disable vim-shell mappings
let g:shell_fullsreen_message = 0 " I know what I'm doing

if &textwidth " Use textwidth if defined; else use 78
    let g:goyo_width = &textwidth
else
    let g:goyo_width = 78
endif
let g:goyo_margin_top = 2
let g:goyo_margin_bottom = 2

let g:gundo_preview_bottom = 1 " Gundo preview takes up full width

let g:EasyMotion_do_mapping = 0 " Disable Easymotion default mappings
let g:EasyMotion_prompt = '{n}/>> '
let g:EasyMotion_keys = 'asdfghjkl;qwertyuiopzxcvbnm'
" }}}
" Part II:  Custom functions {{{
" TODO: Move switch-testing to subfunctions?
function! Rulerer() "{{{ A better ruler
    function! s:r() "{{{
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
    endfunction "}}}
    let R = s:r()

    function! s:myperc(nr)
        if a:nr == 0
            return 'Top'
        elseif a:nr == 100
            return 'Bot'
        else
            return printf('%2d%%', a:nr)
        endif
    endfunction "
    let R.myperc = s:myperc(R.perc)

    return R
endfunction "}}}
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
function! MyFoldText() "{{{
    " Custom text on folds
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 4
    let foldedlinecount = printf('%3d', v:foldend - v:foldstart)

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = substitute(line, '^\s*', repeat('-', v:foldlevel - 1).' ', '')

    let foldchars = substitute(&fmr, ',.*', '', '')
    let line = substitute(line, foldchars, '', '')
    let commentstr = substitute(&cms, '^\(.*\)%s\(.*\)', '\1\|\2', '')
    let commentstr = substitute(commentstr, '|$', '', '')
    let commentstr = substitute(commentstr, '\([\[\]\$\^\.\*|\\]\)',
                                \ '\\\1', 'g')
    let line = substitute(line, commentstr, '', '')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)

    let line = line . repeat(' ', fillcharcount)
    return line . v:foldlevel . '> ' .foldedlinecount .  ' '
endfunction "}}}
function! Typewriter(switch) " {{{
    " TODO: break out into plugin (GASP)
    "   - g:typewriter_font = Courier_New (?)
    "   - g:typewriter_colo = peachpuff
    " Typewriter on ----------------------
    if a:switch == 'on'
        let g:typewriter_enabled = 1

        let g:oldgfn  = &guifont
        let g:oldcolo = g:colors_name
        let g:oldbg   = &background
        let g:oldcul  = &cursorline
        let g:oldcc   = &colorcolumn
        let g:oldrnu  = &relativenumber
        let g:oldnu   = &number

        set guifont =Courier_Prime:h11:cANSI
        " colorscheme sweater
        set background=light
        set nocursorline colorcolumn=0
        set norelativenumber number
    " Typewriter off ---------------------
    elseif a:switch == 'off'
        let g:typewriter_enabled = 0

        let &guifont     = exists('g:oldgfn') ? g:oldgfn      : &gfn
        let &background  = exists('g:oldbg')  ? g:oldbg       : &bg
        let &cursorline  = exists('g:oldcul') ? g:oldcul      : &cul
        let &colorcolumn = exists('g:oldcc')  ? g:oldcc       : &cc
        let &relativenumber = exists('g:oldrnu') ? g:oldrnu   : &rnu
        let &number      = exists('g:oldnu')  ? g:oldnu       : &nu
        if exists('g:oldcolo')
            exec "color " . g:oldcolo
        endif
    " Test if enabled --------------------
    elseif a:switch == 'is?'
        if !exists('g:typewriter_enabled')
            let g:typewriter_enabled = 0
        endif
        return g:typewriter_enabled
    " Toggle -----------------------------
    elseif a:switch == 'tog'
        if Typewriter('is?')
            call Typewriter('off')
        else
            call Typewriter('on')
        endif
    endif
endfunction " }}}
function! Status(winnr) "{{{
    let stat = ''
    let active = winnr() == a:winnr
    let buffer = winbufnr(a:winnr)

    let modified = getbufvar(buffer, '&modified')
    let readonly = getbufvar(buffer, '&readonly')
    let fname = bufname(buffer)

    function! Color(active, num, content)
        if a:active && !has('win32')
            return '%' . a:num . '*' . a:content . '%*'
        else
            return a:content
        endif
    endfunction

    " column
    let stat .= '%1*' . (col(".") / 100 >= 1 ? '%v ' : ' %2v ') . '%*'

    " file
    let stat .= ' ' . Color(active, 4, active ? '»' : '›')
    let stat .= ' %<'

    if fname == '__Gundo__'
        let stat .= 'Gundo'
    elseif fname == '__Gundo_Preview__'
        let stat .= 'Gundo Preview'
    elseif fname == ''
        let stat .= '______'
    else
        let stat .= '%f'
    endif

    let stat .= ' ' . Color(active, 4, active ? '«' : '')

    " file modified
    let stat .= Color(active, 2, modified ? ' + ' : '')

    " readonly
    let stat .= Color(active, 2, readonly ?
                \ &ft == 'help' ? ' ? ' : ' ‼ '
                \ : '')

    " paste
    if active && &paste
        let stat .= '%2*' . ' P ' . '%*'
    endif

    " gutter & right side
    let stat .= '%='

    let stat .= '%p%%'

    return stat
endfunction

" --- Status Autocmd
function! SetStatus()
    for nr in range(1, winnr('$'))
        call setwinvar(nr, '&statusline', '%!Status('.nr.')')
    endfor
endfunction

augroup StatusUpdate
    au!
    au VimEnter,WinEnter,BufWinEnter,BufUnload * call SetStatus()
    au BufWritePost $MYVIMRC call SetStatus()
augroup END

" --- Status Colors
hi User1 guifg=#268bd2 gui=bold
hi User2 guifg=#d33682 gui=bold
hi User3 guifg=#719e07 gui=bold
hi User4 guifg=#2aa198 gui=bold
"}}}
" }}}
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
if has('gui_running') || &t_Co>=88
    colorscheme solarized
    set colorcolumn=78         " highlight column 78
    set cursorline             " highlight the line the cursor's on
else                           " 8-color terms can't handle colors
    colorscheme desert
    set colorcolumn=0
    set nocursorline
endif

set laststatus=2               " use status line, always.

set wildmenu           " tab completion with a menu
set ruler              " show ruler
set showcmd            " Show partial commands as-you-type

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
" --- --- ViM Operational Directories {{{
set directory=$HOME/.vim/swap//    " directory for swap files
set backupdir=$HOME/.vim/backup//  " directory for backups
set undodir=$HOME/.vim/undoes//    " directory for UNDO TREE
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
" }}}

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

set nofoldenable      " Disable folding
set foldmethod=marker " {{{ }}} mark folds
set foldlevel=2       " Start open to second level
set foldcolumn=0      " Fold columns in gutter
set foldtext=MyFoldText() " custom function (above)

set gdefault          " default to global (line) substitutions
set magic             " use better regexp

" set textwidth=78
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

" remove search highlight
nnoremap <leader>/ :nohlsearch<return><Esc>

" Remove whitespace from the ends of lines
nnoremap <silent><leader>rs mz:%s/\s\+$//<CR>:let @/=''<CR>`z
" Remove blank lines
noremap <silent><leader>rb :g/^$/d<CR>
" --- --- }}}
" --- --- Function keybinds {{{
nnoremap <leader>wc :echo 'words: ' . Rulerer().words.tot<CR>
nnoremap <silent> <F10> :call Typewriter('tog')<CR>
nnoremap gt :call NextTabOrBuffer(1)<CR>
nnoremap gT :call NextTabOrBuffer(-1)<CR>
" --- --- }}}
" --- --- Plugin keybinds {{{
nnoremap <F11>   :Goyo<CR>
nnoremap <S-F11> :Fullscreen<CR>
nnoremap <F5>    :GundoToggle<CR>
vnoremap \|      :Tabularize /
let g:user_emmet_leader_key = '<c-e>'
nnoremap <C-o>   :CtrlPMRU<CR>

nmap f <Plug>(easymotion-f)
nmap t <Plug>(easymotion-t)
omap f <Plug>(easymotion-f)
omap t <Plug>(easymotion-t)
nmap F <Plug>(easymotion-F)
nmap T <Plug>(easymotion-T)
omap F <Plug>(easymotion-F)
omap T <Plug>(easymotion-T)
nmap <Leader>, <Plug>(easymotion-repeat)
" --- --- }}}
" --- }}}
" }}}
" Part V:   Autocommands {{{
" Change local current directory to buffer's directory
autocmd BufEnter * silent! lcd %:p:h
augroup GoyoEvents "{{{
    " Goyo fullscreens
    autocmd!
    autocmd User GoyoEnter Limelight | set nocursorline
    autocmd User GoyoLeave Limelight! |
                \ if !Typewriter('is?') |
                \     set cursorline | endif
augroup END "}}}
augroup TextEditing "{{{
    " For texty filetypes
    autocmd!
    autocmd BufNewFile,BufRead *.md set ft=markdown spell
    autocmd FileType vimwiki,markdown,text setlocal spell
    " TODO: switch back when not a texty filetype
augroup END "}}}
augroup ReloadVimrc "{{{
    " Source $MYVIMRC on save
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END "}}}
augroup CurLine "{{{
    " Only show cursorline in current window + normal mode
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * if !Typewriter('is?') |
                              \ set cursorline | endif
augroup END "}}}
augroup Trailing "{{{
    " Only show trailing spaces when out of insert mode
    au!
    au InsertEnter * :match none '\s\+$'
    au InsertLeave * :match ErrorMsg '\s\+$'
augroup END " }}}
augroup ft_help "{{{
    au!
    au FileType help setlocal nospell
    au BufWinEnter *.txt
                \ if &ft == 'help' && winwidth(0) >= 2 * 78 |
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
        au VimEnter * let &columns = &textwidth
                    \       ? &textwidth + &fdc + &nu * &nuw
                    \       : 78 + &fdc + &nu * &nuw
        au VimEnter * set lines=36
    augroup END
    " --- Set fonts for different systems
    if has("gui_gtk2")
        set guifont=Inconsolata
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
