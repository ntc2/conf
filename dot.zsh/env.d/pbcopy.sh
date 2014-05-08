# Emulate the OS X pbcopy and pbpaste commands, for programmatically
# copying and pasting.
#
# From https://coderwall.com/p/kdoqkq

# Use the C-c / C-v clipboard:

# alias pbcopy='xsel --clipboard --input'
# alias pbpaste='xsel --clipboard --output'

# Save to both the C-c / C-v clipboard (called '--clipboard') and the
# highlight / middle mouse clipboard (called '--primary') when copying
# stdin.
alias nc:pbcopy:stdin='tee >(xsel --input --primary) | xsel --input --clipboard'
# Separate commands for reading from the highlight clipboard and C-c /
# C-v clipboard (since you generally highlight before doing C-c, the
# 'nc:pbpaste:primary' often works for both, but not from Firefox to
# Emacs in some fancy Javascripty text fields ... since that's the
# only case I seem to care about in practice, a better solution would
# be probably to ensure that C-c in Firefox copied to all clipboards
# ...):
alias nc:pbpaste:primary='xsel --output --primary'
alias nc:pbpaste:clipboard='xsel --output --clipboard'
# Separate commands for duplicating the highlight and C-c / C-v
# clipboards to both clipboards.
alias nc:pbcopy:primary='nc:pbpaste:primary | nc:pbcopy:stdin'
alias nc:pbcopy:clipboard='nc:pbpaste:clipboard | nc:pbcopy:stdin'
