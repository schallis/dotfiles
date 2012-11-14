" .vimrc
" Steve Challis
" wget schallis.com/vimrc -O ~/.vimrc

" Stop long messages
set shortmess=atI

" Custom leader
let mapleader=","

" typo abbreviations
abbr hte the
abbr scalabality scalability
abbr compatability compatibility
abbr accross across
abbr corresspondence correspondence

set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

Bundle 'scrooloose/nerdtree'
Bundle 'flazz/vim-colorschemes'
Bundle 'wincent/Command-T'
Bundle 'tpope/vim-surround'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-rake'
Bundle 'Lokaltog/vim-powerline'
Bundle 'jiangmiao/auto-pairs'
Bundle 'tpope/vim-endwise'
Bundle 'matchit.zip'
Bundle 'kana/vim-textobj-user'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'othree/html5.vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'leshill/vim-json'
Bundle 'michaeljsmith/vim-indent-object'
if version >= 730
    Bundle 'myusuf3/numbers.vim'
    Bundle 'sontek/rope-vim'
endif
Bundle 'sjl/gundo.vim'

" Use hidden buffers rather than closing them
" set hidden

" Use local bash_profile
set shell=bash\ --login"

" Map to something useful
map H ^
map L $
cmap [ :tabp
cmap ] :tabn

" Remove scrollbar/menu/tabs/etc
set guioptions=a
set mousehide

" Colours
if has('syntax')
    syntax on
    colorscheme zenburn
endif

" Only use one space when joining lines
set nojoinspaces"

" Basics
set tw=78
"set wrap

set autoindent
set smartindent
set smarttab
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4

set ruler
set nohls
set showmatch
set incsearch
set ignorecase
set smartcase

set history=1000
set undolevels=1000
set wildignore=*.swp,*.bak,*.pyc,*.class,*.obj
set title
set visualbell
set noerrorbells
set cursorline
" Show options during tab completion"
set wildmenu
" Always show the statusline
set laststatus=2
" Necessary to show unicode glyphs
set encoding=utf-8
" Make backup files
set backup
" Location of backup files
set backupdir=~/.vim/backup
" Fuck swap files
set noswapfile
set wildmode=list:longest,full
set scrolloff=5

" Highlight 80th column so code can still be pretty in full-screen terminals
if exists("&colorcolumn")
    set colorcolumn=81
endif"



" GVim / MacVim Config ============================================ {{{

" Special options for gvim (instead of in .gvimrc)
let s:uname = system("echo -n \"$(uname)\"")

if has("gui_running")
"    " Light solarized colour scheme for gvim
"    set background=light
"    colorscheme solarized
    " Make the window a bit taller
    set columns=80
    set lines=45
    " Kill toolbar
    set guioptions-=T
    " Kill scrollbars
    set guioptions-=L
    set guioptions-=r
    " Add a menu option for reloading the vimrc
    menu File.Reload\ Configuration :source ~/.vimrc<CR>:filetype detect<CR>
    " Set a pretty font
    if s:uname == "Darwin"
        set gfn=Menlo\ For\ Powerline:h12
    else
        set guifont=Inconsolata\ 12
    endif
endif

" Stay in visual mode after indenting
vnoremap < <gv
vnoremap > >gv

" Emacs-style start and end of line
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

" Make Y yank rest of line, like D and C
nnoremap Y y$

" Cut, copy and paste using the real clipboard
vnoremap <leader>y "+y
vnoremap <leader>x "+x
nnoremap <leader>p "+gp
nnoremap <leader>P "+gP

" Substitute
nnoremap <leader>s :%s//g<left><left>

" Emacs bindings in command line mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" Less chording
nnoremap ; :

" Quick return in insert mode
inoremap <c-cr> <esc>A<cr>
inoremap <s-cr> <esc>O

" Space toggles folds
nnoremap <space> za
vnoremap <space> za

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" Switch tabs easily
if s:uname == "Darwin"
    nnoremap <D-S-left> gT
    nnoremap <D-S-right> gt
else
    nnoremap <A-S-left> gT
    nnoremap <A-S-right> gt
endif

" Fullscreen
if s:uname == "Darwin"
    noremap <D-Enter> :se invfullscreen<CR>
    inoremap <D-Enter> <ESC>:set invfullscreen<CR>a
else
    noremap <A-Enter> :se invfullscreen<CR>
    inoremap <A-Enter> <ESC>:set invfullscreen<CR>a
end

" Shortcuts for enabling / disabling search highlighting
nnoremap ,hl :set hls<CR>
nnoremap ,nhl :set nohls<CR>

" Use alt + {j,k} for moving lines up and down
if s:uname == "Darwin"
    nnoremap <D-j> :m+<CR>==
    nnoremap <D-k> :m-2<CR>==
    "inoremap <M-j> <Esc>:m+<CR>==gi
    "inoremap <M-k> <Esc>:m-2<CR>==gi
    vnoremap <D-j> :m'>+<CR>gv=gv
    vnoremap <D-k> :m-2<CR>gv=gv
else
    nnoremap <A-j> :m+<CR>==
    nnoremap <A-k> :m-2<CR>==
    "inoremap <A-j> <Esc>:m+<CR>==gi
    "inoremap <A-k> <Esc>:m-2<CR>==gi
    vnoremap <A-j> :m'>+<CR>gv=gv
    vnoremap <A-k> :m-2<CR>gv=gv
endif

" Write files with sudo if opened without priviliedges
cmap w!! w !sudo tee % >/dev/null

" }}}

" Plugin Options ================================================== {{{

if s:uname == "Darwin"
    " Command-T -- open in new tab with Command-Enter
    let g:CommandTAcceptSelectionTabMap=['<D-CR>', '<C-t>']
endif

" Posh powerline glyphs
let g:Powerline_symbols = 'fancy'
" Don't let Rails status line conflict with powerline
let g:rails_statusline = 0

" Make delimate handle spacing better
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1

" }}}

" Autocommands ==================================================== {{{

" JSON support
au! BufRead,BufNewFile *.json setfiletype json

if has("autocmd")
    " Clean up whitespace on save
    autocmd BufWritePre * CleanWhitespace
    " Tell ruby files to use two spaces for indentation
    autocmd FileType ruby setlocal softtabstop=2 shiftwidth=2 tabstop=4
    " Tell json files to use two spaces for indentation
    autocmd FileType json setlocal softtabstop=2 shiftwidth=2 tabstop=4
    " Tell javascript files to use two spaces for indentation
    autocmd FileType javascript setlocal softtabstop=2 shiftwidth=2 tabstop=4
    " Tell coffeescript files to use two spaces for indentation
    autocmd FileType coffee setlocal softtabstop=2 shiftwidth=2 tabstop=4
    " Tell scala files to use two spaces for indentation
    autocmd FileType scala setlocal softtabstop=2 shiftwidth=2 tabstop=4
    " Makefiles use tabs only
    autocmd FileType make setlocal noexpandtab
    " Some types of files should wrap to 79 characters
    autocmd FileType tex setlocal textwidth=79
    autocmd FileType plaintex setlocal textwidth=79
    autocmd FileType latex setlocal textwidth=79
    autocmd FileType rst setlocal textwidth=79
    " Enable spell checking for latex and rst
    autocmd FileType tex setlocal spell spelllang=en_gb
    autocmd FileType plaintex setlocal spell spelllang=en_gb
    autocmd FileType latex setlocal spell spelllang=en_gb
    autocmd FileType rst setlocal spell spelllang=en_gb
    " Use pdflatex for compiling latex files
    autocmd FileType tex setlocal makeprg=pdflatex\ %
    " Don't do things like indent lines following lines that start with 'for'
    autocmd FileType tex setlocal nosmartindent
    autocmd FileType plaintex setlocal nosmartindent
    autocmd FileType latex setlocal nosmartindent
    " Enable omnicomplete for Python
    "autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    " No wrapping for Python
    autocmd FileType python setlocal nowrap
    " Auto compile coffeescript
    autocmd BufWritePost,FileWritePost *.coffee :silent !coffee -c <afile>

    " Enable Markdown support
    autocmd FileType markdown setlocal ai formatoptions=tcroqn2 comments=n:&gt;
    autocmd FileType markdown setlocal nolist linebreak

    " Use {{{ - }}} style folds in vimscript
    autocmd FileType vim setlocal foldmethod=marker
endif

" }}}

" Misc ============================================================ {{{

" Remove trailing whitespace
function! <SID>CleanWhitespace()
    " Preparation - save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction
command CleanWhitespace call <SID>CleanWhitespace()


" Source a global configuration file if available
if filereadable(expand("$HOME/.vimrc.local"))
    source $HOME/.vimrc.local
endif

" }}}

se nu

"if has('filetype')
    " Filetype options
    filetype on
    filetype plugin indent on

    " Filetype defs
    let filetype_m='objc'
    let filetype_emacs='lisp'
"endif

" Highlight whitespace
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" Toggle list visibility
nmap <silent> <leader>s :set nolist!<CR>
vmap <silent> <leader>c :s/^/#/<CR>

" Emulate emacs fillprefix (requires external plugin)
nmap <silent> <leader>fp :call SetQuotePrefixFromCursor()<CR>

" Remain compatible with vim version which do not have autocmd
if has('autocmd')
    autocmd BufRead *json set nowrap
    autocmd BufRead *json set tw=0
    autocmd BufRead *html set nowrap
    autocmd BufRead *html set tw=0
    autocmd BufRead *html set noautoindent
    autocmd BufRead *html set listchars-=tab:>
    "autocmd BufRead *py set foldmethod=indent
    "autocmd FileType python     call FT_python()
    "autocmd FileType python compiler pylint
    autocmd! BufRead,BufNewFile *.org set filetype=org
endif

"" Prevent noob behaviour
"map <up> <nop>
"map <down> <nop>
"map <left> <nop>
"map <right> <nop>
"inoremap <up> <nop>
"inoremap <down> <nop>
"inoremap <left> <nop>
"inoremap <right> <nop>

" Change long line cursor behavior
nnoremap j gj
nnoremap k gk

" Clear search with :/
nmap <silent> :/ :nohlsearch<CR>

" Allow writing of root owned files with w!!
cmap w!! w !sudo tee % >/dev/null

" Org mode stuff
map <Tab> za

au! BufRead,BufNewFile *.org
set filetype=org

" Fold options
set foldmethod=indent
set foldlevelstart=20

"function! FT_python()
"    set omnifunc=pythoncomplete#Complete
"endfunction

" Scroll viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" relative numbers asre slow on large monitors
" se nornu

function! NyanMe() " {{{
    hi NyanFur             guifg=#BBBBBB
    hi NyanPoptartEdge     guifg=#ffd0ac
    hi NyanPoptartFrosting guifg=#fd3699 guibg=#fe98ff
    hi NyanRainbow1        guifg=#6831f8
    hi NyanRainbow2        guifg=#0099fc
    hi NyanRainbow3        guifg=#3cfa04
    hi NyanRainbow4        guifg=#fdfe00
    hi NyanRainbow5        guifg=#fc9d00
    hi NyanRainbow6        guifg=#fe0000


    echohl NyanRainbow1
    echon "≈"
    echohl NyanRainbow2
    echon "≋"
    echohl NyanRainbow3
    echon "≈"
    echohl NyanRainbow4
    echon "≋"
    echohl NyanRainbow5
    echon "≈"
    echohl NyanRainbow6
    echon "≋"
    echohl NyanRainbow1
    echon "≈"
    echohl NyanRainbow2
    echon "≋"
    echohl NyanRainbow3
    echon "≈"
    echohl NyanRainbow4
    echon "≋"
    echohl NyanRainbow5
    echon "≈"
    echohl NyanRainbow6
    echon "≋"
    echohl None
    echo ""

    echohl NyanRainbow1
    echon "≈"
    echohl NyanRainbow2
    echon "≋"
    echohl NyanRainbow3
    echon "≈"
    echohl NyanRainbow4
    echon "≋"
    echohl NyanRainbow5
    echon "≈"
    echohl NyanRainbow6
    echon "≋"
    echohl NyanRainbow1
    echon "≈"
    echohl NyanRainbow2
    echon "≋"
    echohl NyanRainbow3
    echon "≈"
    echohl NyanRainbow4
    echon "≋"
    echohl NyanRainbow5
    echon "≈"
    echohl NyanRainbow6
    echon "≋"
    echohl NyanFur
    echon "╰"
    echohl NyanPoptartEdge
    echon "⟨"
    echohl NyanPoptartFrosting
    echon "⣮⣯⡿"
    echohl NyanPoptartEdge
    echon "⟩"
    echohl NyanFur
    echon "⩾^ω^⩽"
    echohl None
    echo ""

    echohl NyanRainbow1
    echon "≈"
    echohl NyanRainbow2
    echon "≋"
    echohl NyanRainbow3
    echon "≈"
    echohl NyanRainbow4
    echon "≋"
    echohl NyanRainbow5
    echon "≈"
    echohl NyanRainbow6
    echon "≋"
    echohl NyanRainbow1
    echon "≈"
    echohl NyanRainbow2
    echon "≋"
    echohl NyanRainbow3
    echon "≈"
    echohl NyanRainbow4
    echon "≋"
    echohl NyanRainbow5
    echon "≈"
    echohl NyanRainbow6
    echon "≋"
    echohl None
    echon " "
    echohl NyanFur
    echon "”   ‟"
    echohl None

    sleep 3
    redraw
    echo " "
    echo " "
    echo "Noms?"
    redraw
endfunction " }}}
command! NyanMe call NyanMe()
