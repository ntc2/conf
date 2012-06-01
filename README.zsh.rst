=================
ZSH Configuration
=================

This dir contains a modular zsh environment.  The appropriate symlinks
are created when you run ``../install-conf.py``.

There are three main user local zsh conf files.

``~/.zshenv``
    Loaded for all shells. Links to ``./dot.zshenv`` and loads ``~/.zsh/env.d/*`` and ``~/.zshenv.system-custom``.

``~/.zshrc``
    Loaded for interactive shells. Links to ``./dot.zshrc`` and loads ``~/..zsh/rc.d/*``  and ``~/.zshrc.system-custom``.

``~/.zprofile``
    Loaded for login shells.

See http://zsh.sunsite.dk/Guide/zshguide02.html#l9 for more details.

My criterion for the breakup into ``env`` or ``rc`` is: put it in env
unless it's totally pointless or might cause problems.  And only
worrying about problems might be even better...

The ``man zsh`` says::

  STARTUP/SHUTDOWN FILES
         Commands  are  first  read from /etc/zsh/zshenv; this
         cannot be overridden.  Subsequent behaviour is  modi‐
         fied  by  the  RCS and GLOBAL_RCS options; the former
         affects all startup  files,  while  the  second  only
         affects  global  startup files (those shown here with
         an path starting with a /).  If one of the options is
         unset at any point, any subsequent startup file(s) of
         the corresponding type will not be read.  It is  also
         possible   for   a  file  in  $ZDOTDIR  to  re-enable
         GLOBAL_RCS.  Both  RCS  and  GLOBAL_RCS  are  set  by
         default.

         Commands are then read from $ZDOTDIR/.zshenv.  If the
         shell is  a  login  shell,  commands  are  read  from
         /etc/zsh/zprofile and then $ZDOTDIR/.zprofile.  Then,
         if the shell is interactive, commands are  read  from
         /etc/zsh/zshrc and then $ZDOTDIR/.zshrc.  Finally, if
         the shell  is  a  login  shell,  /etc/zsh/zlogin  and
         $ZDOTDIR/.zlogin are read.

         When a login shell exits, the files $ZDOTDIR/.zlogout
         and then /etc/zsh/zlogout  are  read.   This  happens
         with  either  an explicit exit via the exit or logout
         commands, or an implicit exit by reading  end-of-file
         from  the terminal.  However, if the shell terminates
         due to exec'ing another process, the logout files are
         not  read.   These  are  also affected by the RCS and
         GLOBAL_RCS options.  Note also that  the  RCS  option
         affects  the  saving of history files, i.e. if RCS is
         unset when the shell exits, no history file  will  be
         saved.

         If  ZDOTDIR  is  unset,  HOME is used instead.  Files
         listed above as being  in  /etc  may  be  in  another
         directory, depending on the installation.

         As  /etc/zsh/zshenv  is run for all instances of zsh,
         it is important that it be kept as small as possible.
         In  particular,  it  is  a good idea to put code that
         does not need to be run for every single shell behind
         a  test  of  the  form `if [[ -o rcs ]]; then ...' so
         that it will not be executed when zsh is invoked with
         the `-f' option.

         Any of these files may be pre-compiled with the zcom‐
         pile builtin command (see zshbuiltins(1)).  If a com‐
         piled  file  exists (named for the original file plus
         the .zwc extension) and it is newer than the original
         file, the compiled file will be used instead.

  FILES
         $ZDOTDIR/.zshenv
         $ZDOTDIR/.zprofile
         $ZDOTDIR/.zshrc
         $ZDOTDIR/.zlogin
         $ZDOTDIR/.zlogout
         ${TMPPREFIX}*   (default is /tmp/zsh*)
         /etc/zsh/zshenv
         /etc/zsh/zprofile
         /etc/zsh/zshrc
         /etc/zsh/zlogin
         /etc/zsh/zlogout     (installation-specific - /etc is
         the default)
