" .vimrc
"
" Steve Challis
" stevechallis.com
"
" Do not use these without understading *all* of
" the commands, they may do unexpected things!

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

" Why bother limiting the feature set for compatability with vi
set nocompatible

" Use hidden buffers rather than closing them
set hidden

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
    colorscheme desert
endif

" Basics
set tw=78
set wrap

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
set wildignore=*.swp,*.bak,*.pyc,*.class
set title
set visualbell
set noerrorbells
set cursorline
set wildmenu
set wildmode=list:longest,full
set scrolloff=5

" Turn off backup files
set nobackup
set noswapfile


if has('filetype')
    " Filetype options
    filetype on
    filetype plugin on
    filetype indent on

    " Filetype defs
    let filetype_m='objc'
    let filetype_emacs='lisp'
endif

" Highlight whitespace
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" Toggle list visibility
nmap <silent> <leader>s :set nolist!<CR>

" Emulate emacs fillprefix (requires external plugin)
nmap <silent> <leader>fp :call SetQuotePrefixFromCursor()<CR>

" Remain compatible with vim version which do not have autocmd
if has('autocmd')
    autocmd BufRead *html set nowrap
    autocmd BufRead *html set tw=0
    autocmd BufRead *html set noautoindent
    autocmd BufRead *html set listchars-=tab:>
    "autocmd BufRead *py set foldmethod=indent
    "autocmd FileType python     call FT_python()
    autocmd FileType python compiler pylint
    autocmd! BufRead,BufNewFile *.org set filetype=org
endif

" Prevent noob behaviour
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

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
