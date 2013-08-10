# Emulate the OS X pbcopy and pbpaste commands, for programmatically
# copying and pasting.
#
# From https://coderwall.com/p/kdoqkq

# Use the C-c / C-v clipboard:

# alias pbcopy='xsel --clipboard --input'
# alias pbpaste='xsel --clipboard --output'

# Use both the C-c / C-v and the highlight / middle mouse clipboard
# when saving; read from the highlight clipboard (but, since you
# generally highlight before doing C-c, this works for both!):
alias nc:pbcopy='tee >(xsel --input --primary) | xsel --input --clipboard'
alias nc:pbpaste='xsel --output --primary'
