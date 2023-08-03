from bs4 import BeautifulSoup
import glob
import os
import zipfile
import sys
import re


def pretty_title(file):
    f = open(file, "r", encoding="utf8")
    soup = BeautifulSoup(f, 'xml')
    return soup.title.text


def atoi(text):
    return int(text) if text.isdigit() else text


def natural_keys(text):
    '''
    alist.sort(key=natural_keys) sorts in human order
    http://nedbatchelder.com/blog/200712/human_sorting.html
    (See Toothy's implementation in the comments)
    '''
    return [atoi(c) for c in re.split(r'(\d+)', text)]


def html_files(folder):
    if not folder.endswith("/"):
        folder += "/"
    files = [file for file in glob.glob(f"{folder}*.xhtml")]
    files.sort(key=natural_keys)
    return files


def generate(html_files, novelname, author, chapter_s, chapter_e):
    epub = zipfile.ZipFile(novelname + "_" + chapter_s +
                           "-" + chapter_e + ".epub", "w")
    epub.writestr("META-INF/container.xml", '''<container version="1.0"
    xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
      <rootfiles>
        <rootfile full-path="OEBPS/Content.opf" media-type="application/oebps-package+xml"/>
      </rootfiles>
    </container>''')

    index_tpl = '''<package version="3.1"
    xmlns="http://www.idpf.org/2007/opf">
      <metadata>
        %(metadata)s
          </metadata>
            <manifest>
              %(manifest)s2
            </manifest>
            <spine>
              <itemref idref="toc" linear="no"/>
              %(spine)s
            </spine>
    </package>'''

    manifest = ""
    spine = ""
    metadata = '''<dc:title xmlns:dc="http://purl.org/dc/elements/1.1/">%(novelname)s</dc:title>
      <dc:creator xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ns0="http://www.idpf.org/2007/opf" ns0:role="aut" ns0:file-as="NaN">%(author)s</dc:creator>
        <meta xmlns:dc="http://purl.org/dc/elements/1.1/" name="calibre:series" content="%(series)s"/>''' \
    % {
        "novelname": novelname + ": " + chapter_s + "-" + chapter_e, "author": author, "series": novelname}
    toc_manifest = '<item href="toc.xhtml" id="toc" properties="nav" media-type="application/xhtml+xml"/>'

    toc_mid = ""
    for i, file in enumerate(html_files):
        basename = os.path.basename(file)
        manifest += '<item id="file_%s" href="%s" media-type="application/xhtml+xml"/>' % (
            i + 1, basename)
        spine += '<itemref idref="file_%s" />' % (i + 1)
        epub.write(file, "OEBPS/" + basename)
        toc_mid += f'<li class="toc-Chapter-rw" id="num_{i}"><a href="{basename}">{pretty_title(file)}</a></li>\n'

    epub.writestr("OEBPS/Content.opf", index_tpl % {
                  "metadata": metadata,
                  "manifest": manifest + toc_manifest,
                  "spine": spine, })

    toc_start = '''<?xml version='1.0' encoding='utf-8'?>
    <!DOCTYPE html>
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
        <head>
            <title>%(novelname)s</title>
        </head>
            <body>
                <section class="frontmatter TableOfContents">
            <header>
                <h1>Contents</h1>
            </header>
                <nav id="toc" role="doc-toc" epub:type="toc">
                    <ol>
                        %(toc_mid)s
                        %(toc_end)s'''
    toc_end = '''</ol></nav></section></body></html>'''

    epub.writestr("OEBPS/toc.xhtml", toc_start %
                  {"novelname": novelname, "toc_mid": toc_mid, "toc_end": toc_end})
    epub.close()


print('Usage:\npython epub-ify.py "folder/" "title" "author"\n')

folder = sys.argv[1]
title = sys.argv[2]
author = sys.argv[3]

files = html_files(folder)

# this looks for the first number in the file name
first_chapter = re.search(r'[1-9][0-9]*', os.path.basename(files[0])).group(0)
last_chapter = re.search(r'[1-9][0-9]*', os.path.basename(files[-1])).group(0)

generate(html_files(folder), title, author, first_chapter, last_chapter)
