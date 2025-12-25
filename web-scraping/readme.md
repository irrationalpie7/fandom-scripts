# Webnovel downloader

This is mostly a convenience thing for me, so it's not well-documented, sorry!

There's two main python scripts:

1. `webnovel-downloader.py`: Download the chapters of a webnovel as separate xhtml files in a folder. Even if you get it running, you will probably have to tweak the script to get it to download the particular webnovel you want.
   - Note: I'm in the process of splitting this up into two files, `webnovel-scrape.py` to prefetch the raw html, and `webnovel-parse.py` to clean the html. This makes the scraping a lot simpler to set up for a new site, and will theoretically also help with making the parsing less network-bound, which is often a real bottleneck (in particular if the site in question imposes rate limits). At the moment, however, `webnovel-parse` doesn't exist yet, and `webnovel-downloader` can only hackily work from a downloaded folder
2. `epub-ify.py`: Turn a folder of chapter files into an epub. Uses `<title>` of each file as the chapter title, and assumes files are numbered in increasing order, with extra zeroes to make 01 come before 10, for example. These zeroes may be optional but I'm not positive.

## Usage overview

### `webnovel-scrape.py`

Prereq: if you need to install playwright, you'll need to cd into this dir, then:

```py
python3 -m venv .venv
source .venv/bin/activate
pip install pytest-playwright
playwright install
pip install bs4
```

Later, just `source .venv/bin/activate` will be sufficient.

Example command:
`python3 webnovel-scrape.py https://wtr-lab.com/en/novel/19458/game-invasion-starting-with-a-random-system-draw max=376 out_dir=raw-html-4`

Note: the script can use playwright to scrape sites which require javascript to load the text, but it's pretty slow, and if the site has any sort of scrape detection it may stop working (check the outputted `error-file-i.html` for a security check or similar)

### `webnovel-downloader.py`

The basic usage is like so:

```
python3 webnovel-downloader.py <url> <number of chapters/toc pages; defaults to 1> <num chapters to download; defaults to 1; set to "all" to override>
```

1. url: The url of the web novel or table of contents. The format will differ by website
2. number of chapters/toc pages: The number of chapters/table-of-contents pages that will be accessed. If you need to edit the script to handle a different table-of-contents format, this can limit the number of table-of-contents pages pulled while you're working out the details.
3. number of chapters to download: The number of chapters to download. A table-of-contents page might have multiple chapter links on it, this is a way to limit the actual number of chapters downloaded in that scenario. Use "all" instead of a number to get all chapters

If you run this successfully, it'll spit out something like:

```
Fetching novel from: <link>
Saving in chapters-#/
```

It'll pick a new number for the chapters folder each time so you can re-run the script without clobbering previous runs (although you may wish to delete old folders once you no longer need them).

#### Supported sites

(at least as of when I needed the support, which was a while ago):

1. https://novelfull.com/ : uses the novel overview page as the table of contents base
2. https://www.wuxiaworld.eu : chapters have urls ending in "/chapter/novel-name-#", so provide "https://www.wuxiaworld.eu/chapter/novel-name-" as the url and specify the correct number of chapters
3. https://username.tumblr.com/post/numbers/name-of-post : this assumes that the linked post contains an ordered list of links to story chapters, which are themselves tumblr posts

You may notice that these all make some rather brittle assumptions and that if the websites make any changes the script will stop working---yep!! I have no bandwidth to maintain these, I just wanted _a_ way to download a story the first time, and then got a request to download a different story off a different website and was like, surely this can't be too different. It was, unfortunately, but there was commonality and each time it gets a little easier/faster to modify the script to do the specific thing I want it to do and then ignore it until I need it again, probably for yet another website.

### `epub-ify.py`

```
python3 epub-ify.py folder-name/ "Title of story" "Author name"
```

This should spit out an epub file based on the chapters in the folder. It'll include the chapter range in the epub file name (e.g. 1-42). I think this is because a previous version of this script was designed to make epubs in batches if there were too many chapters. I think I modified the script at least a little, but I definitely got the base code from somewhere and I don't remember where unfortunately.
