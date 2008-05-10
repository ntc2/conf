__doc__ = '''Start an IPython shell (for debugging).  Recall that imports only
happen once, unless you use reload(), so when you import ipydb in a
loop IPython only gets started once.  Import this module where ever
you want the IPython shell to be started, e.g.

def foo(bar):
    for x in bar:
        if baz(x):
            import ipydb # <-- start IPython here, with current value of x.
'''
import inspect
__up_frame = inspect.currentframe().f_back
import IPython
eval('IPython.Shell.IPShellEmbed([])()', # Empty list arg is ipythons argv
     __up_frame.f_globals,
     dict(__up_frame.f_locals.items() + locals().items()))
# Exit right away.
#import sys
#sys.exit(1)
