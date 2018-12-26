#!/usr/bin/env python2

import glob, os, os.path, sys

path_template = '%s/.stack/programs/x86_64-linux/ghc-%%s/bin' % os.getenv('HOME')

def all_ghc_paths():
    for p in sorted(glob.glob(path_template % '*')):
        if os.path.isdir(p):
            yield p

def ghc_ver_to_path(ghc_ver):
    """Convert a GHC version number, e.g. 8.2.1, to a Stack bin path.

    Die with error if requested GHC is not installed."""
    path = path_template % ghc_ver
    if not os.path.exists(path):
        print >> sys.stderr, "No such GHC: %s!" % path
        print >> sys.stderr, "Available GHC versions:"
        for p in all_ghc_paths(): print >> sys.stderr, '\t'+p
        print >> sys.stderr, "Perhaps you need to run 'stack setup %s'?" % ghc_ver
        sys.exit(2)
    return path

def main(ghc_ver, path):
    """Add GHC version 'ghc_ver' to path, removing any existing (known) GHCs from path."""
    paths = [p for p in path.split(':') if p not in all_ghc_paths()]
    paths = [ghc_ver_to_path(ghc_ver)] + paths
    print ':'.join(paths)

if __name__ == '__main__':
    import sys
    main(sys.argv[1], sys.argv[2])
