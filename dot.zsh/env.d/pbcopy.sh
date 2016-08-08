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
#
# UPDATE: this just hangs for me now; probably related to some ZSH
# change I made since defining this. However, it was only used to copy
# one clibboard to another, so below I just replaced it with
# overwriting the destination clipboard, since there's no point in
# overwriting the source clipboard with it's current content.
alias nc:pbcopy:stdin='tee >(xsel --input --primary) | xsel --input --clipboard'
# Separate commands for reading from the highlight clipboard and C-c /
# C-v clipboard (since you generally highlight before doing C-c, the
# 'nc:pbpaste:primary' often works for both, but not from Firefox to
# Emacs in some fancy Javascripty text fields ... since that's the
# only case I seem to care about in practice, a better solution would
# be probably to ensure that C-c in Firefox copied to all clipboards
# ...):
alias nc:pbpaste:highlight='xsel --output --primary'
alias nc:pbpaste:ctrlc='xsel --output --clipboard'
# Separate commands for duplicating the highlight and C-c / C-v
# clipboards to both clipboards.
alias nc:pbcopy:highlight='nc:pbpaste:highlight | xsel --input --clipboard'
alias nc:pbcopy:ctrlc='nc:pbpaste:ctrlc | xsel --input --primary'
