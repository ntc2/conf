#!/usr/bin/env python
"""
Print command you can use to make a DVD with mkisofs and growisofs.

Run with -h for options.

I seem to have a bunch of trouble with DMA turning off sporadically.
Through much trial and error I have recently been seeing good results
with premastering the image and then burning it with dao.  However, I
have still had weird stuff happen with that process, so maybe I have
just had better luck with that process.  In any case IF THE DVD DRIVE
SPINS UP REPEATEDLY, AND MAKES A WEIRD CLICK/SMACK NOISE AT THE END OF
EACH SPIN UP, kill the growisofs process with ^C or ^Z then kill.  The
DVD won't get hosed until the buring actually starts, but it seems
that whenever the weird spin up stuff happens, the DVD is a coaster.

All testing done with TDK DVD-R 8x blanks.
"""
import os

class DVDCommands(object):
    def __init__(self, ops):
        self.ops = ops
        self.set_files(ops.f)
        
    # filename -> [(filename,basename)]
    def set_files(self, f):
        files = []
        for s in (s.strip() for s in  open(f).readlines()):
            # Skip blank and comments
            if not s or s[0] == '#':
                continue
            # Split and quote names
            files.append(('"%s"' % s, '"%s"' % os.path.basename(s)))
        self.files = files

    # [(filename,basename)] -> IO()
    def print_sum(self):
        full_names = " ".join(f for (f,_) in self.files)
        os.system("du -B 1000 -sc %s" % full_names)
        os.system("du --si -sc %s | grep total" % full_names)

    # [(filename,basename)] -> DVD :)
    def burn_dvd(self):
        # Use graft points, and a special dir for torrents
        graft_points = []
        for (n,b) in self.files:
            # Everything got quoted in get_files()
            if b.endswith('.torrent"'):
                prefix = "torrents/"
            else:
                prefix = ""
            graft_points.append("%s%s=%s" % (prefix, b, n))
        graft = " ".join(graft_points)
        iso_file = os.path.basename(self.ops.f)+".iso"
        iso_t = "sudo nice -n -5 mkisofs -o %(iso_file)s -J -r -joliet-long -graft-points %(graft)s"
        print "Copy and paste commands:"
        print
        print
        print iso_t % locals()
        grow_t = "sudo hdparm /dev/dvd; echo sleeping for 5 seconds ...; sleep 5; \
sudo nice -n -5 growisofs -use-the-force-luke=dao -Z /dev/dvd=%(iso_file)s"
        print
        print
        print  grow_t % locals()
        

    # [(filename,basename)] -> A whole lot of nothing
    def remove_files(self):
        a = raw_input("Are you sure you want to remove:\n%s\n[y/N]: " \
                      % "\n".join([f for (_,f) in self.files]))
        if not a or not a.lower()[0] == "y":
            print "Aborting"
            import sys
            sys.exit()
        for (f,_) in self.files:
            f = f[1:-1] # Chop off the quotes
            if os.path.exists(f):
                print "Removing", f
                if os.path.isdir(f):
                    import shutil
                    shutil.rmtree(f)
                else:
                    os.remove(f)
            else:
                print f, "doesn't exist.  Skipping..."

def main(ops):
    dvd = DVDCommands(ops)
    if ops.s:
        dvd.print_sum()
    if ops.b:
        dvd.burn_dvd()
    if ops.remove_files:
        dvd.remove_files()

if __name__ == '__main__':
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option('-s', action="store_true", default=False,
                      help="Sums the size of the input files.")
    parser.add_option('-f', help="""
File containing input files, one per line.  Lines starting with # will be ignored.""")
    parser.add_option('-b', help="""
Burn a DVD using growisofs with files specified via -f argument at top
level""", action="store_true", default=False)
    parser.add_option('--remove-files', help="""Remove all files in file specified with -f.""",
                      action="store_true", default=False)
    ops,args = parser.parse_args()
    if not ops.f:
        print "Error:  You must specify a file with -f"
    else:
        main(ops)
