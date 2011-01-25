"
" Script: syntax/org.vim
"
" Version: 2.1
"
" Description:
"
"       This uses folding to mimic outline-mode in Emacs.  Set the filetype to
"       'outline' to activate this functinality.  Put it in the modeline to
"       preserve outlines between edits.  For example:
"
"               vim:set ft=outline:
"
" Installation:
"
"   Place this file in your home directory under ~/.vim/syntax/, or in the
"   system location under the syntax/ directory to provide it to all users.
"
" Maintainer: Tye Z. <zdro@yahoo.com>
"       modified by Jean-Marie Gaillourdet <jmg@gaillourdet.net>
"
" Customization:
"
"   The colors of the outline headers can be changed by linking them to
"   whatever you like.  For example:
"
"       hi! link Outline_1 Statement
"
" History:
"
"   Version 2.1: (Jean-Marie Gaillourdet)
"
"       - Renamed to org.vim
"       - Added rules to match headings up to level 20
"       - Added rule to highlight and fold drawers
"
"   Version 2.0:
"
"       - Rewritten to use syntax-folding instead of a fold expression
"       - Eliminated slowness in folds with large amounts of text
"
"   Version 1.2:
"
"       - Updated regexes for readability, using a number for the '*' count
"
"   Version 1.1:
"
"       - Initial version
"


"
" Do folding with syntax.  This works pretty darn well, and is simpler than a
" complicated foldexpr.
"
" - An outline block starts with a '*' and ends when approaching another '*'
"   at the beginning of a line.
"
" - An outline block can contain any outline block of a lower level.  So, a
"   level 3 can be inside a level 1 without the intermediary level 2.
"
" - A '_' can end a block, allowing one to insert extra space between two
"   folds.
"

" ****************************************************************************
" *                                        Syntax Regions for Outline Header *
" ****************************************************************************
" {{{ 
syn region Outline_1 matchgroup=Outline_1_match
            \ start='^\*[^*][^:]*'
            \ end='\n\ze\n\?\*[^*]\|^<$'
            \ contains=Tags
            \ fold keepend

syn region Outline_2 matchgroup=Outline_2_match
            \ start='^\*\{2\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,2\}[^*]\|^<\{2\}$'
            \ containedin=Outline_1,Outline_2
            \ contains=Tags
            \ fold keepend

syn region Outline_3 matchgroup=Outline_3_match
            \ start='^\*\{3\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,3\}[^*]\|^<\{3\}$'
            \ containedin=Outline_1,Outline_2,Outline_3
            \ contains=Tags
            \ fold keepend

syn region Outline_4 matchgroup=Outline_4_match
            \ start='^\*\{4\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,4\}[^*]\|^<\{4\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4
            \ contains=Tags
            \ fold keepend

syn region Outline_5 matchgroup=Outline_5_match
            \ start='^\*\{5\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,5\}[^*]\|^<\{5\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5
            \ contains=Tags
            \ fold keepend

syn region Outline_6 matchgroup=Outline_6_match
            \ start='^\*\{6\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,6\}[^*]\|^<\{6\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6
            \ contains=Tags
            \ fold keepend

syn region Outline_7 matchgroup=Outline_7_match
            \ start='^\*\{7\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,7\}[^*]\|^<\{7\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7
            \ contains=Tags
            \ fold keepend

syn region Outline_8 matchgroup=Outline_8_match
            \ start='^\*\{8\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,8\}[^*]\|^<\{8\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8
            \ contains=Tags
            \ fold keepend

syn region Outline_9 matchgroup=Outline_9_match
            \ start='^\*\{9\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,9\}[^*]\|^<\{9\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9
            \ contains=Tags
            \ fold keepend

syn region Outline_10 matchgroup=Outline_10_match
            \ start='^\*\{10\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,10\}[^*]\|^<\{10\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10
            \ contains=Tags
            \ fold keepend

syn region Outline_11 matchgroup=Outline_11_match
            \ start='^\*\{11\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,11\}[^*]\|^<\{11\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11
            \ contains=Tags
            \ fold keepend

syn region Outline_12 matchgroup=Outline_12_match
            \ start='^\*\{12\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,12\}[^*]\|^<\{12\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12
            \ contains=Tags
            \ fold keepend

syn region Outline_13 matchgroup=Outline_13_match
            \ start='^\*\{13\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,13\}[^*]\|^<\{13\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13
            \ contains=Tags
            \ fold keepend

syn region Outline_14 matchgroup=Outline_14_match
            \ start='^\*\{14\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,14\}[^*]\|^<\{14\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14
            \ contains=Tags
            \ fold keepend

syn region Outline_15 matchgroup=Outline_15_match
            \ start='^\*\{15\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,15\}[^*]\|^<\{15\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15
            \ contains=Tags
            \ fold keepend

syn region Outline_16 matchgroup=Outline_16_match
            \ start='^\*\{16\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,16\}[^*]\|^<\{16\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15,Outline_16
            \ contains=Tags
            \ fold keepend

syn region Outline_17 matchgroup=Outline_17_match
            \ start='^\*\{17\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,17\}[^*]\|^<\{17\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15,Outline_16,Outline_17
            \ contains=Tags
            \ fold keepend

syn region Outline_18 matchgroup=Outline_18_match
            \ start='^\*\{18\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,18\}[^*]\|^<\{18\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15,Outline_16,Outline_17,Outline_18
            \ contains=Tags
            \ fold keepend

syn region Outline_19 matchgroup=Outline_19_match
            \ start='^\*\{19\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,19\}[^*]\|^<\{19\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15,Outline_16,Outline_17,Outline_18,Outline_19
            \ contains=Tags
            \ fold keepend

syn region Outline_20 matchgroup=Outline_20_match
            \ start='^\*\{20\}[^*][^:]*'
            \ end='\n\ze\n\?\*\{1,20\}[^*]\|^<\{20\}$'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15,Outline_16,Outline_17,Outline_18,Outline_19,Outline_20
            \ contains=Tags
            \ fold keepend
" }}}


" ****************************************************************************
" *                                                Syntax Regions for Drawer *
" ****************************************************************************
" {{{
syn region DRAWER matchgroup=Drawer_match
            \ start='^ *:[^:]*:'
            \ end='^ *:END:'
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15,Outline_16,Outline_17,Outline_18,Outline_19,Outline_20
            \ fold
" }}}

" ****************************************************************************
" *                                                     Syntax for Meta Data *
" ****************************************************************************
" {{{
syn match Configuration '^\#\+.*$'

syn match Tags contained ':[^ ]*:'
" TODO: try to make it match only in a headline

syn match MetaData_Date 
            \ containedin=Outline_1,Outline_2,Outline_3,Outline_4,Outline_5,Outline_6,Outline_7,Outline_8,Outline_9,Outline_10,Outline_11,Outline_12,Outline_13,Outline_14,Outline_15,Outline_16,Outline_17,Outline_18,Outline_19,Outline_20
            \ '\(DEADLINE\|SCHEDULED\):' 

syn match ActiveDate 
            \ containedin=ALL 
            \ '<[0-9]\{4,4}-[0-9]\{2,2}-[0-9]\{2,2}\s\+\(Mon\|Tue\|Wed\|Thu\|Fri\|Sat\|Sun\)\?>'

syn match InActiveDate
            \ containedin=ALL 
            \ '\[[0-9]\{4,4}-[0-9]\{2,2}-[0-9]\{2,2}\s\+\(Mon\|Tue\|Wed\|Thu\|Fri\|Sat\|Sun\)\?\]'
" }}}

""" Debugging: Hilight the whole block so we can see exactly what is being
"""            done.
"hi Outline_1 guibg=gray40
"hi Outline_2 guibg=#330000
"hi Outline_3 guibg=#003300
"hi Outline_4 guibg=#000033
"" Disable
""hi Outline_1 guibg=bg
""hi Outline_2 guibg=bg
""hi Outline_3 guibg=bg
""hi Outline_4 guibg=bg

hi! default link Outline_1_match Comment
hi! default link Outline_2_match Identifier
hi! default link Outline_3_match PreProc
hi! default link Outline_4_match Type
hi! default link Outline_5_match Type
hi! default link Outline_6_match Type
hi! default link Outline_7_match Type
hi! default link Outline_8_match Type
hi! default link Outline_9_match Type
hi! default link Outline_10_match Type
hi! default link Outline_11_match Type
hi! default link Outline_12_match Type
hi! default link Outline_13_match Type
hi! default link Outline_14_match Type
hi! default link Outline_15_match Type
hi! default link Outline_16_match Type
hi! default link Outline_17_match Type
hi! default link Outline_18_match Type
hi! default link Outline_19_match Type
hi! default link Outline_20_match Type

hi! default link Drawer_match Comment

hi! default link Tags Todo

hi! default link Configuration PreProc

hi! default link MetaData_Date Keyword
hi! default link ActiveDate Constant
hi! default link InActiveDate Constant

syn sync fromstart
setlocal foldmethod=syntax

finish

