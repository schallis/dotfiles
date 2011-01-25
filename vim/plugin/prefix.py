function! FillPrefix()
py << EOF
import vim

cur_line = vim.current.line

if not cur_line:
    prefix = '' # Unset prefix if called on empty line
else:
    prefix = 'n:' + cur_line.split(' ')[0]

vim.command('set comments=%s' % prefix)
EOF
endfunction

function! SetQuotePrefixFromCursor() 
python << EOF
import vim

cursor_col = vim.current.window.cursor[1]
quote_prefix = vim.current.line[:cursor_col]

if quote_prefix:
   set_cmd = "set comments=n:%s" % quote_prefix
else:
   set_cmd = "set comments=" # Cancel quoting prefix

vim.command(set_cmd)
EOF
endfunction
