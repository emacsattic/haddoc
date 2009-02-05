"""
Lookup some search terms in the index.
"""

import sys
import haddoc
from collections import defaultdict


def main():
    import optparse
    parser = optparse.OptionParser(__doc__.strip())
    haddoc.add_options(parser)
    opts, args = parser.parse_args()
    haddoc.validate_options(opts)

    if not args:
        parser.error("You need to specify some search terms.")
    
    conn = haddoc.getconnection()
    curs = conn.cursor()
    rsets = []
    curs.execute("""
       SELECT term, source, fullname, url FROM terms
       WHERE term ~* %s
    """, ('.*'.join(args),))

    res = defaultdict(list)
    for term, source, fullname, url in curs:
        res[url].append((source, fullname))

    for url, rlist in res.iteritems():
        print url
        for source, fullname in rlist:
            print "    %s: %s" % (source, fullname)
        print

    conn.close()


