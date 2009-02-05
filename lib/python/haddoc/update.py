"""
Update the database of search terms to URLs.
"""

from __future__ import with_statement
import sys, re, urllib, formatter

from BeautifulSoup import BeautifulSoup, Tag

import haddoc
from haddoc import dbapi, committing



def main():
    import optparse
    parser = optparse.OptionParser(__doc__.strip())
    haddoc.add_options(parser)
    opts, args = parser.parse_args()
    haddoc.validate_options(opts)

    if len(args) != 2:
        parser.error("This program requires the source type name and the name "
                     "of the index file to process.")
    source, fn = args

    conn = haddoc.getconnection()

    results = process_index(fn)
    for terms, url in results:
        fullterm = ' / '.join(terms)
        words = []
        for term in terms:
            nterm = cleanup(term)
            words.extend(x.strip() for x in nterm.split())
        term = ' '.join(words)
        with committing(conn) as curs:
            curs.execute("""
               INSERT INTO terms (term, source, fullname, url)
               VALUES (%s, %s, %s, %s);
            """, (term, source, fullterm, url))
        print repr(fullterm)

    conn.close()

def cleanup(term):
    term = re.sub('\(.+\)', '', term)
    term = re.sub('\[.*\]', '', term)
    term = re.sub(',', '', term)
    return term



def process_index(url):
    """
    Parse an index filename into a dictionary of name -> URL.
    """
    try:
        htfile = urllib.urlopen(url)
        text = htfile.read()
        htfile.close()
    except IOError, e:
        raise SystemExit("Error: fetching file from the web, '%s'" % e)

    try:
        parser = IndexProcessor()
        parser.feed(text)
        parser.close()
    except IOError, e:
        raise SystemExit("Error: parsing file, '%s'" % e)

    return parser.results


import htmllib
class IndexProcessor(htmllib.HTMLParser):
    """
    Extract the index links from a Python HTML documentation index.
    """
    def __init__(self):
        htmllib.HTMLParser.__init__(self, formatter.NullFormatter())
        self.words = []
        self.dtlevel = 0
        self.last = None
        self.results = []
        self.saved = None

    def handle_data(self, data):
        if self.saved is not None:
            self.saved += data

    def start_dt(self, att):
        self.dtlevel += 1
        self.saved = self.saved or ''

    def end_dt(self):
        self.dtlevel -= 1
        if self.saved:
            self.last, self.saved = self.saved, None
            self.url = None


    def start_a(self, att):
        d = dict(att)
        self.url = d['href']
        self.saved = self.saved or ''

    def end_a(self):
        url, self.url = self.url, None
        self.last, self.saved = self.saved, None
        self.results.append((self.words + [self.last], url))


    def start_dd(self, att):
        self.words.append(self.last)

    def end_dd(self):
        self.words.pop()


