#!/usr/bin/env python
#
# Install script for haddoc.
#

__version__ = "$Revision$"
__author__ = "Martin Blais <blais@furius.ca>"

from distutils.core import setup

def read_version():
    try:
        return open('VERSION', 'r').readline().strip()
    except IOError, e:
        raise SystemExit(
            "Error: you must run setup from the root directory (%s)" % str(e))

setup(name="haddoc",
      version=read_version(),
      description=\
      "Browse Python HTML Documentation from Emacs",
      long_description="""
Haddoc is a simple tool that allows an emacs user to search the Python HTML
documentation indexes and to bring a web browser to an index term page.
""",
      license="GPL",
      author="Martin Blais",
      author_email="blais@furius.ca",
      url="http://furius.ca/haddoc",
      package_dir = {'': 'lib/python'},
      packages = ['haddoc'],
      scripts = ['bin/haddoc-update', 'bin/haddoc']
     )

