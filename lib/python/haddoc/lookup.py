"""
Lookup some search terms in the index.
"""

import sys
import haddoc

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
       WHERE term ~ %s
    """, ('.*'.join(args),))
    for term, source, fullname, url in curs:
        print '%s;%s' % (fullname, url)
    conn.close()

