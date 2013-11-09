# Make 'less' read the whole file so that the percentage from '-M'
# works.
#
# See http://stackoverflow.com/a/19871578/470844.
export MANPAGER='less -s -M +Gg'
