from xml.etree.ElementTree import canonicalize
from bs4 import BeautifulSoup
import urllib.request
import sys
import re


def canonical(origurl, relativeurl):
    m = re.search(r'https?://[^/]*', relativeurl)
    if m:
        return relativeurl
    m = re.search(r'https?://[^/]*', origurl)
    return f"{m.group(0)}{relativeurl}"


def clearTag(tag, origurl):
    del tag['class']
    del tag['style']
    for child in tag.findChildren():
        if child.has_attr('href'):
            child["href"] = canonical(origurl, child["href"])
        clearTag(child, origurl)


# table of contents:
# https://novelfull.com/reincarnation-of-the-strongest-sword-god.html
# https://novelfull.com/reincarnation-of-the-strongest-sword-god.html?page=2
# ...
# https://novelfull.com/reincarnation-of-the-strongest-sword-god.html?page=64
# "a" descendents of "list-chapter" will contain links to chapters
#
# per chapter:
# title: .chapter-title
# content: #chapter-content

def get_soup(url):
    user_agent = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.7) Gecko/2009021910 Firefox/3.0.7'
    headers = {'User-Agent': user_agent, }
    request = urllib.request.Request(
        url, None, headers)  # The assembled request
    response = urllib.request.urlopen(request)
    return BeautifulSoup(response.read(), "lxml")


def get_urls(url):
    soup = get_soup(url)
    return [canonical(url, x["href"]) for x in soup.select('.list-chapter a')]


def download_chapters(offset, url):
    urls = get_urls(url)
    for i, chapter_url in enumerate(urls):
        download_chapter(chapter_url, i+offset)


def remove_tags(content):
    # remove ads and such
    for data in content.select('script, .ads, .adsbygoogle'):
        # Remove tags
        data.decompose()

    return content


def download_chapter(url, chapter_i, chapter_content_select="#chapter-content", chapter_title_select=".chapter-title"):
    soup = get_soup(url)
    title = soup.select(chapter_title_select, limit=1)[0].text
    content = ''.join(
        f"{remove_tags(x).prettify()}\n" for x in soup.select(chapter_content_select))

    file = open(f"chapters/chapter-{chapter_i+1}.xhtml", "w", encoding="utf8")
    file.write('<html xmlns="http://www.w3.org/1999/xhtml">')
    file.write("\n<head>")
    file.write("\n<title>" + title + "</title>")
    file.write("\n</head>")
    file.write("\n<body>")
    file.write("\n<strong>" + title + "</strong>" + "\n")
    file.write(content)
    file.write("\n</body>")
    file.write("\n</html>")
    file.close()


print('Usage:\npython webnovel-downloader.py https://novelfull.com/reincarnation-of-the-strongest-sword-god.html\nNote that this script makes a couple assumptions about\nformat and such based on this original link.')
url = sys.argv[1]
if not url.startswith("http"):
    sys.exit()
else:
    print(f"Fetching: {url}")

download_chapters(0, url)
print("50 / 3159")

for i in range(2, 65):
    download_chapters(i*50-50, f"{url}?page={i}")
    print(f"{50*i} / 3159")
