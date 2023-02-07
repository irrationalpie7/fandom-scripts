from bs4 import BeautifulSoup, NavigableString
import urllib.request
import sys
import re


def canonical(orig_url, relative_url):
    m = re.search(r'https?://[^/]*', relative_url)
    if m:
        return relative_url
    m = re.search(r'https?://[^/]*', orig_url)
    return f"{m.group(0)}{relative_url}"


def get_soup(url):
    # this sets a non-robot user agent so we don't get blocked
    user_agent = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.7) Gecko/2009021910 Firefox/3.0.7'
    headers = {'User-Agent': user_agent, }
    request = urllib.request.Request(
        url, None, headers)  # The assembled request
    response = urllib.request.urlopen(request)
    return BeautifulSoup(response.read(), "lxml")


def remove_style_attribute(element):
    if isinstance(element, NavigableString):
        return
    if element.has_attr('style'):
        element['data-orig-style'] = element['style']
        del element['style']


def clean(content, orig_url):
    # remove ads, scripts, and styles
    for data in content.select('script, style, .ads, .adsbygoogle'):
        data.decompose()

    # remove styles specified as attributes
    remove_style_attribute(content)
    for descendant in content.descendants:
        remove_style_attribute(descendant)

    # totally empty filler elements (and their otherwise empty parents)
    # can go bye bye
    for data in content.select('div, p, span'):
        # (no text or child elements)
        while data.text.strip() == "" and len(data.find_all()) == 0:
            parent = data.parent
            data.decompose()
            data = parent

    # technically if it's linking to another chapter or something it makes
    # more sense to link within the epub, but eh.
    for data in content.select('[href]'):
        # this is a half-assed attempt to not break footnotes
        if not data["href"].startswith('#'):
            data["href"] = canonical(orig_url, data["href"])

    return content


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


def get_chapter_urls(toc_url, chapter_url_selector='.list-chapter a'):
    soup = get_soup(toc_url)
    return [canonical(toc_url, x["href"]) for x in soup.select(chapter_url_selector)]

# 1-216, but for some reason there's no 106:
# https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-#
def get_chapter_urls_wuxiaworld(base_url):
    return [f"{base_url}{x}" for x in range(1, 106)] + [f"{base_url}{x}" for x in range(107, 217)]

def download_chapters(offset, multi_chapter_url, chapter_select=".chapter"):
    # alternative that has the url of a page with the contents of multiple
    # chapters on it
    soup = get_soup(multi_chapter_url)
    chapters = soup.select(chapter_select)
    for i, chapter_soup in enumerate(chapters):
        save_chapter(chapter_soup, multi_chapter_url, i+offset)
        if i+offset % 50 == 49:
            print(f"Processed {i+offset+1} chapters")
    return offset + len(chapters)

def download_chapters_toc(offset, toc_url):
    # alternative that has the url of a table of contents
    urls = get_chapter_urls_wuxiaworld(toc_url)
    for i, chapter_url in enumerate(urls):
        save_chapter(get_soup(chapter_url), chapter_url, i+offset)
        if i+offset % 50 == 49:
            print(f"Downloaded {i+offset} chapters")
    return offset + len(urls)

# For https://novelfull.com/reincarnation-of-the-strongest-sword-god.html
def extract_chapter_info_novelfull(chapter_soup, url):
    chapter_content_select="#chapter-content"
    chapter_title_select=".chapter-title"

    title = chapter_soup.select(chapter_title_select, limit=1)[0].text
    content = '\n'.join(
        f"{clean(x, url).prettify()}" for x in chapter_soup.select(chapter_content_select))
    return [title, content]

# https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-# for 1-216, -106
def extract_chapter_info_wuxiaworld(chapter_soup, url):
    chapter_content_select="#chapterText"
    chapter_title_select="h1"

    title = chapter_soup.select(chapter_title_select, limit=1)[0].text
    content = '\n'.join(
        f"{clean(x, url).prettify()}" for x in chapter_soup.select(chapter_content_select))
    content = re.sub("<div[^>]*>", "<p>", content).replace("</div>","</p>")
    content = re.sub("<p>\s*</p>", "", content)
    return [title, content]

def save_chapter(chapter_soup, url, chapter_i):

    chapter_info = extract_chapter_info_wuxiaworld(chapter_soup, url)
    title = chapter_info[0]
    content = chapter_info[1]

    file = open(
        f"chapters-3/chapter-{chapter_i+1:06}.xhtml", "w", encoding="utf8")

    file.write("<?xml version='1.0' encoding='utf-8'?>\n")
    file.write('<html xmlns="http://www.w3.org/1999/xhtml">\n')
    file.write(f"<head>\n<title>{title}</title>\n</head>\n")
    file.write(
        f"<body>\n<h1>{title}</h1>\n{content}\n</body>\n</html>")
    file.close()

# chapter urls:
# https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-# for 1-216, -106
# chapter text: Array.from(document.querySelectorAll("#chapterText")).map(div => div.innerHTML)

print('Usage:\npython webnovel-downloader.py https://novelfull.com/reincarnation-of-the-strongest-sword-god.html\nNote that this script makes a couple assumptions about\nformat and such based on this original link.')
url = sys.argv[1]
if not url.startswith("http"):
    sys.exit()
else:
    print(f"Fetching: {url}")

download_chapters_toc(0, url)

# toc_urls = [url]
# for i in range(2, 65):
#     toc_urls.append(f"{url}?page={i}")

# offset = 0
# for url in toc_urls:
#     offset = download_chapters_toc(offset, url)
