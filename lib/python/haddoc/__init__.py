#!/usr/bin/env python

"""
Haddoc library for looking up entries from the Python index to a web browser
from emacs.  All the code is here.  The scripts are just simple hooks that call
code in this library.
"""

# stdlib imports
import os, re, anydbm, urllib, htmllib, formatter, itertools
from os.path import *
from contextlib import contextmanager

# dbapi imports
import psycopg2 as dbapi

# haddoc imports
import haddoc





#-------------------------------------------------------------------------------

def_rc = join(os.environ['HOME'], '.haddocrc')

config = {
    'host': 'localhost',
    'database': 'haddoc',
    'user': '',
    'password': '',
    }

def add_options(parser):
    """
    Add the necessary options for haddoc programs.
    """
    parser.add_option(
        '-C', '--config', action='store',
        default=os.environ.get('HADDOC_CONFIG', def_rc),
        help="Location of configuration file.")

def validate_options(opts):
    if opts.config and exists(opts.config):
        load_configuration(opts.config)

def load_configuration(fn):
    env = {}
    try:
        execfile(fn, env)
        config.update((k, v) for (k, v) in env.iteritems() if k in config)
    except Exception, e:
        raise SystemExit("Error reading configution file: %s" % e)

#-------------------------------------------------------------------------------

_connection = None

def getconnection():
    global _connection
    if _connection is None:
        _connection = dbapi.connect(**config)
    return _connection

@contextmanager
def committing(conn, ignore_errors=False):
    try:
        yield conn.cursor()
    except dbapi.Error, e:
        conn.rollback()
        if not ignore_errors:
            raise e
    else:
        conn.commit()
