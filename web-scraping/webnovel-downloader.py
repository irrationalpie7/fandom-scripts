from bs4 import BeautifulSoup, NavigableString
import urllib.request
import sys
import re
import os


class Chapter(object):
    """Represents a chapter. Must always have an associated url,
    but may not initially have a chapter title or associated BeautifulSoup
    """
    # Constructor

    def __init__(self, url: str, title: str = None, soup: BeautifulSoup = None):
        self.url = url
        self.title = title
        self.soup = soup

    def get_soup(self) -> BeautifulSoup:
        if self.soup is None:
            self.soup = get_soup(self.url)
        return self.soup

def set_chapter_titles_by_index(chapters: list[Chapter]):
    for i, chapter in enumerate(chapters):
        chapter.title = f"Chapter {i}"


class WebnovelDownloader(object):
    # Get list of chapter urls
    def get_chapter_urls(self) -> list[str]:
        return []

    def extract_chapter_title(self, chapter_soup) -> str:
        return ""

    def extract_chapter_content(self, chapter_soup, chapter_url: str) -> str:
        return ""

    # override this if a single url has multiple chapters in it
    def get_soups(self, chapter_url) -> list:
        return [get_soup(chapter_url)]

    def setup_download(self, url_batch_size=50):
        self.url_offset = 0
        self.downloaded_chapters = 0
        self.url_batch_size = url_batch_size
        # TODO: setup download dir
        self.file_dir = "chapters"
        i = 4
        while os.path.exists(f"{self.file_dir}-{i}"):
            i += 1
        self.file_dir = f"{self.file_dir}-{i}"
        os.mkdir(self.file_dir)
        print(f"Saving in {self.file_dir}/")

    def download(self, num=-1, url_batch_size=50):
        if num > 0:
            url_batch_size = min(num, url_batch_size)
        self.setup_download(url_batch_size)
        while not self.done_downloading():
            self.download_batch()
            if num > 0:
                if self.downloaded_chapters >= num:
                    return
                if num - self.url_offset < self.url_batch_size:
                    self.url_batch_size = num - self.url_offset

    def done_downloading(self) -> bool:
        return self.url_offset >= len(self.get_chapter_urls())

    def download_batch(self) -> int:
        """Downloads the next batch of chapters
        """
        if self.url_batch_size <= 0:
            print("Must call setup_download with positive batch size first")
            return
        urls = self.get_chapter_urls(
        )[self.url_offset:self.url_offset+self.url_batch_size]
        for chapter_url in urls:
            for chapter_soup in self.get_soups(chapter_url):
                try:
                    self.save_chapter(chapter_soup, chapter_url,
                                      self.downloaded_chapters)
                except IndexError as err:  # Exception
                    print(f"Unable to download chapter at: {chapter_url}")
                    print(err)
                self.downloaded_chapters += 1
                if (self.downloaded_chapters) % 50 == 0:
                    print(
                        f"... so far, tried {self.downloaded_chapters} chapter urls...")
            self.url_offset += 1

    def save_chapter(self, chapter_soup, chapter_url, chapter_i: int):
        try:
            title = self.extract_chapter_title(chapter_soup)
            content = self.extract_chapter_content(chapter_soup, chapter_url)
        except:
            print(f"Unable to parse chapter at {chapter_url}")
            err_file = f"{self.file_dir}/err/chapter-{chapter_i+1:06}.xhtml"
            print(f"Attempting to write it to {err_file}")
            if not os.path.exists(f"{self.file_dir}/err"):
                os.mkdir(f"{self.file_dir}/err")
            with open(err_file, "w", encoding="utf8") as file:
                file.write(chapter_soup.prettify())
            return

        with open(f"{self.file_dir}/chapter-{chapter_i+1:06}.xhtml", "w", encoding="utf8") as file:
            file.write("<?xml version='1.0' encoding='utf-8'?>\n")
            file.write('<html xmlns="http://www.w3.org/1999/xhtml">\n')
            file.write(f"<head>\n<title>{title}</title>\n</head>\n")
            file.write(
                f"<body>\n<h1>{title}</h1>\n{content}\n</body>\n</html>")


class ParameterizedDownloader(WebnovelDownloader):
    # Constructor
    def __init__(self, toc_url: str, toc_selector: str, chapter_text_selector: str, chapter_parent_selector: str, include_toc: bool):
        """Encapsulates toc-like downloading options.
        @toc_url: E.g. https://shanastoryteller.tumblr.com/post/724074957212172288/happy-pride-fem-mxy-wwx-pls
        @toc_selector: e.g. ".captext a"
        @chapter_text_selector: e.g. .captext
        """
        self.toc_url = toc_url
        self.toc_selector = toc_selector
        self.chapter_text_selector = chapter_text_selector
        self.chapter_parent_selector = chapter_parent_selector
        self.chapter_urls = []
        self.chapter_number = 0
        self.include_toc = include_toc

    def get_chapter_urls(self) -> list[str]:
        # cache this so we don't need to do num_toc_pages web requests every time
        if self.chapter_urls == []:
            self.chapter_urls = get_chapter_urls_from_toc(
                self.toc_url, self.toc_selector)
            if self.include_toc:
                self.chapter_urls = self.chapter_urls + [self.toc_url]
        return self.chapter_urls

    def extract_chapter_title(self, chapter_soup):
        self.chapter_number += 1
        return f"Chapter {self.chapter_number}"

    def extract_chapter_content(self, chapter_soup, chapter_url: str):
        content = '\n'.join(
            f"{clean(x, chapter_url).prettify()}" for x in chapter_soup.select(self.chapter_text_selector, limit=1))
        return content

    # optionally expands the text at a url into multiple chapters
    # should still work for single chapters
    def get_soups(self, chapter_url) -> list:
        return [x for x in get_soup(chapter_url).select(
            self.chapter_parent_selector)]


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
class NovelfullDownloader(WebnovelDownloader):
    # Constructor
    def __init__(self, toc_url: str, num_toc_pages: int):
        """Encapsulates novelfull.com-specific downloading options.
        @toc_url: E.g. https://novelfull.com/reincarnation-of-the-strongest-sword-god.html
        @num_toc_pages: e.g. 65
        """
        self.toc_url = toc_url
        self.num_toc_pages = num_toc_pages
        self.chapter_urls = []

    def get_chapter_urls(self) -> list[str]:
        # cache this so we don't need to do num_toc_pages web requests every time
        if self.chapter_urls == []:
            chapter_urls = []
            for toc_url in [f"{self.toc_url}?page={x}" for x in range(1, self.num_toc_pages+1)]:
                chapter_urls.extend(get_chapter_urls_from_toc(
                    toc_url, ".list-chapter a"))
            self.chapter_urls = chapter_urls
        return self.chapter_urls

    # For https://novelfull.com/reincarnation-of-the-strongest-sword-god.html
    def extract_chapter_title(self, chapter_soup):
        return chapter_soup.select(".chapter-title", limit=1)[0].text

    # For https://novelfull.com/reincarnation-of-the-strongest-sword-god.html
    def extract_chapter_content(self, chapter_soup, chapter_url: str):
        content = '\n'.join(
            f"{clean(x, chapter_url).prettify()}" for x in chapter_soup.select("#chapter-content"))
        return content


# chapter urls:
# https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-# for 1-216, -106
# chapter text: Array.from(document.querySelectorAll("#chapterText")).map(div => div.innerHTML)
class WuxiaWorldEuDownloader(WebnovelDownloader):
    """Encapsulates www.wuxiaworld.eu-specific downloading options.
    """

    # Constructor
    def __init__(self, base_chapter_url: str, chapters: int):
        """Encapsulates www.wuxiaworld.eu-specific downloading options.
        @base_chapter_url: E.g. https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-
        @chapters: number of chapters, e.g. 216)
        """
        # https://www.wuxiaworld.eu/novel/the-e-sports-circles-toxic-assembly-camp
        # to:
        # https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-
        if "www.wuxiaworld.eu/novel/" in base_chapter_url:
            base_chapter_url = f"{base_chapter_url.replace('/novel/','/chapter/')}-"
        self.base_chapter_url = base_chapter_url
        self.chapters = chapters

    def get_chapter_urls(self) -> list[str]:
        # chapters are at:
        # https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-#
        # so base url is:
        # https://www.wuxiaworld.eu/chapter/the-e-sports-circles-toxic-assembly-camp-
        return [f"{self.base_chapter_url}{x}" for x in range(1, self.chapters+1)]

    def extract_chapter_title(self, chapter_soup) -> str:
        return chapter_soup.select("h1", limit=1)[0].text

    def extract_chapter_content(self, chapter_soup, chapter_url: str) -> str:
        # for some reason all the chapter paragraphs are actually divs with id #chapterText
        content = '\n'.join(
            f"{clean(x, chapter_url).prettify()}" for x in chapter_soup.select("#chapterText"))
        content = re.sub("<div[^>]*>", "<p>",
                         content).replace("</div>", "</p>")
        content = re.sub("<p>\s*</p>", "", content)
        return content


def canonical(orig_url: str, relative_url: str) -> str:
    m = re.search(r'https?://.*', relative_url)
    if m:
        return relative_url
    m = re.search(r'https?://[^/]*', orig_url)
    return f"{m.group(0)}{relative_url}"


def get_soup(url: str) -> BeautifulSoup:
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


def get_chapter_urls_from_toc(toc_url: str, chapter_url_selector: str) -> list[str]:
    soup = get_soup(toc_url)
    return [canonical(toc_url, x["href"]) for x in soup.select(chapter_url_selector)]


print('Usage:\npython webnovel-downloader.py <url> <number of chapters/toc pages; defaults to 1> <num chapters to download; defaults to 1; set to "all" to override>\n')
url = sys.argv[1]
if not url.startswith("http"):
    sys.exit()
else:
    print(f"Fetching novel from: {url}")

num = 1
if len(sys.argv) >= 3:
    num = int(sys.argv[2])

restrict_downloads = True
download_num = 1
if len(sys.argv) >= 4:
    try:
        download_num = int(sys.argv[3])
    except ValueError:
        restrict_downloads = False


downloader = None
if "www.wuxiaworld.eu" in url:
    downloader = WuxiaWorldEuDownloader(url, num)
elif "novelfull.com" in url:
    downloader = NovelfullDownloader(url, num)
elif "tumblr" in url:
    downloader = ParameterizedDownloader(
        url, ".captext a", ".captext", "li.caption", True)
else:
    print("unsupported url host; please implement a WebnovelDownloader to handle parsing")
if downloader is not None:
    if restrict_downloads:
        downloader.download(download_num)
    else:
        downloader.download()
