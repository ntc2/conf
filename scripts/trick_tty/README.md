Based on http://stackoverflow.com/a/14694983/470844.

We create a version of `isatty` that always returns true, and then use
`LD_PRELOAD` to override the default definition with it. This allows
us to e.g. do

    nc:trick_tty <cmd> | less

and see color in `less`, if

    <cmd>

by itself would have shown color, but

    <cmd> | less

would not, because `<cmd>` uses `isatty` to check if it's stdout is
connected to a tty.

I'm going to start by just compiling the shared lib once and
versioning it. When that causes problems I'll build the shared lib in
`install-conf.py` or something.

It seems that `isatty` is not the only check that programs make;
examining the source code of `script`, provided by `bsdutils` package
on Ubuntu, should reveal the rest of the story.
