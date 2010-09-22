" Why bother limiting the feature set for compatability with vi
set nocompatible

" Use hidden buffers rather than closing them
set hidden

" Map to something useful
map H ^
map L $
cmap [ :tabp
cmap ] :tabn

" Filetype defs
let filetype_m='objc'

" Remove scrollbar/menu/tabs/etc
set guioptions=a
set mousehide

" Colours
syntax on
colorscheme desert

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
set wildmode=list:longest
set scrolloff=5

" Turn off backup files
set nobackup
set noswapfile

" Filetype options
filetype on
filetype plugin on
filetype indent on

" Highlight whitespace
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" Remain compatible with vim version which do not have autocmd
if has('autocmd')
    autocmd filetype html,xml set nowrap
    autocmd filetype html,xml set tw=0
    autocmd filetype html,xml set noautoindent
    autocmd filetype html,xml set listchars-=tab:>.
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
