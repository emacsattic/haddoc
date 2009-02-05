"""
Initialize the schema. Run this directly.
"""

from __future__ import with_statement

import haddoc
from haddoc import dbapi, committing




schema = """

  CREATE TABLE terms (

     term     VARCHAR(64),
     source   VARCHAR(32),
     fullname TEXT,
     url      TEXT

  );

  CREATE INDEX terms_idx ON terms (term);

"""


def main():
    import optparse
    parser = optparse.OptionParser(__doc__.strip())
    haddoc.add_options(parser)
    opts, args = parser.parse_args()
    haddoc.validate_options(opts)

    conn = haddoc.getconnection()
    curs = conn.cursor()
    print 'Dropping'
    with committing(conn, ignore_errors=1) as curs:
        curs.execute("DROP TABLE terms")
    print 'Creating'
    with committing(conn) as curs:
        curs.execute(schema)

if __name__ == '__main__':
    main()
