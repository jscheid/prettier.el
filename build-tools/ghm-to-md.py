#!/usr/bin/env python

from panflute import run_filter, Header, Image, Link, Code, Span, Inline, RawInline, RawBlock, Str
import sys

def ghm_to_md(elem, doc):
    if isinstance(elem, Image):
        return []

    if isinstance(elem, Link) and len(elem.content) == 0:
        return []

    if isinstance(elem, Header):
        if elem.level > 1:
            elem.level -= 1
        else:
            return []

    if isinstance(elem, Code):
        return RawInline("`" + elem.text + "'")

    if isinstance(elem, Str):
        if elem.text == '\\':
            return []

    if isinstance(elem, Link):
        elem.parent.content.insert(elem.index + 1, Span(*elem.content))
        return []

def main(doc=None):
    return run_filter(ghm_to_md, doc=doc)

if __name__ == "__main__":
    main()
