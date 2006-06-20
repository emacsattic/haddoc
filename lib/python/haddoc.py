#!/usr/bin/env python

"""
Haddoc library for looking up entries from the Python index to a web browser
from emacs.
"""

# stdlib imports
import os, re, anydbm, urllib, htmllib, formatter
from os.path import *

# haddoc imports
import haddoc


__all__ = ('add_options', 'update', 'lookup')


#-------------------------------------------------------------------------------
#
def_htmldoc = 'http://python.org/doc/html'
def_htmldoc = 'file:///home/blais/py/trunk/Doc/html'

def_dbfile = '/var/lib/haddoc/haddoc.db'


#-------------------------------------------------------------------------------
#
def add_options(parser):
    """
    Add the necessary options for haddoc programs.
    """
    parser.add_option(
        '-D', '--haddoc-database', action='store_true',
        default=os.environ.get('HADDOC_DATABASE', def_dbfile),
        help="Location of haddoc database filename (default: %s)" % def_dbfile)

#-------------------------------------------------------------------------------
#
def update(dbfilename, htmldoc, oss):
    """
    Parse the HTML index files and process them into a database. 'oss' is an
    output stream to print progress information to.
    """
    print >> oss,  'Output to DB file: %s' % dbfilename

    try:
        dn = dirname(dbfilename)
        if not exists(dn):
            os.makedirs(dn)

        db = anydbm.open(dbfilename, 'n')
    except OSError:
        raise SystemExit("Error: Cannot access DB files.")

    genindex = 'genindex.html'
    for indexfn in [join(htmldoc, x, genindex) for x in
                    ('api', 'lib', 'mac', 'ref', 'dist')]:
        print >> oss, "Processing file '%s'." % indexfn
        process_index(indexfn, db)

    db.close()


def process_index(url, db):
    """
    Parse an index filename into a dictionary of name -> URL.
    """
    dirn = dirname(url)

    try:
        htfile = urllib.urlopen(url)
        text = htfile.read()
        htfile.close()
    except IOError, e:
        raise SystemExit("Error: fetching file from the web: '%s'", e)

    try:
        parser = IndexProcessor(db, dirn)
        parser.feed(text)
        parser.close()
    except IOError, e:
        raise SystemExit("Error: fetching file from the web: '%s'", e)


class IndexProcessor(htmllib.HTMLParser):
    """
    Extract the index links from a Python HTML documentation index.
    """
    def __init__(self, db, dirn):
        htmllib.HTMLParser.__init__(self, formatter.NullFormatter())
        self.db = db
        self.dirn = dirn
        self.do_entry = 0

    def start_dt(self, att):
        self.do_entry = 1

    def start_a(self, att):
        if self.do_entry:
            self.url = join(self.dirn, dict(att)['href'])
            self.save_bgn()

    def end_a(self):
        if self.do_entry:
            name = self.save_end()
            if name != '[Link]':
                self.name = name
            self.db[self.name] = self.url
            self.url = None


#-------------------------------------------------------------------------------
#
def lookup(dbfilename, search_terms):
    """
    Search for some terms in the Python index and print out the results, one per
    line.
    """

    # (This uses a really lame algorithm, but the database is small that it's
    # fast enough that I won't bother with implementing something better.)

    # Read all the keys from the database.
    try:
        db = anydbm.open(dbfilename, 'r')
        keys = db.keys()
    except OSError:
        raise SystemExit("Error: Cannot access DB files.")

    # Find the keys that match the terms.
    results = []
    for term in search_terms:
        results.extend( (x, db[x]) for x in
                         filter(re.compile(re.escape(term), re.I).search,
                                keys) )

    db.close()

    # Sort and return the results.
    results.sort()
    return results


def lookup_print(dbfilename, search_terms, oss):
    """
    Prints out the results of a lookup operation in the format expected by the
    emacs client code.
    """
    for name, url in lookup(dbfilename, search_terms):
        print >> oss, '%s;%s' % (name, url)
    
