#!/usr/bin/env python

from panflute import run_filter, Header, Image
from panflute.elements import RAW_FORMATS

def ghm_to_texi(elem, doc):
    if isinstance(elem, Image):
        return []

    if isinstance(elem, Header):
        if elem.level > 1:
            elem.level -= 1
        else:
            return []

def main(doc=None):
    return run_filter(ghm_to_texi, doc=doc)

if __name__ == "__main__":
    RAW_FORMATS.add(u'texinfo')
    main()
