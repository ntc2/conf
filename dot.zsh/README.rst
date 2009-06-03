=================
ZSH Configuration
=================

This dir contains a modular zsh environment.  The appropriate symlinks
are created when you run ``../install-conf.py``.

There are three main user local zsh conf files.

``~/.zshenv``
    Loaded for all shells. Links to ``./dot.zshenv`` and loads ``./env.d/*``.

``~/.zshrc``
    Loaded for interactive shells. Links to ``./dot.zshrc`` and loads ``./rc.d/*``.

``~/.zshprofile``
    Loaded for login shells. Don't have one currently ...

See http://zsh.sunsite.dk/Guide/zshguide02.html#l9 for more details.

The criterion for the breakup into ``env`` or ``rc`` is: put it in env
unless it's totally pointless or might cause problems.  And only
worrying about problems might be even better...
