# Webnovel downloader

This is mostly a convenience thing for me, so it's not well-documented, sorry!

There's two main python scripts:
1. `webnovel-downloader.py`: Download the chapters of a webnovel as separate xhtml files in a folder. Even if you get it running, you will probably have to tweak the script to get it to download the particular webnovel you want.
2. `epub-ify.py`: Turn a folder of chapter files into an epub. Uses `<title>` of each file as the chapter title, and assumes files are numbered in increasing order, with extra zeroes to make 01 come before 10, for example. These zeroes may be optional but I'm not positive.


## Usage overview

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

You may notice that these all make some rather brittle assumptions and that if the websites make any changes the script will stop working---yep!! I have no bandwidth to maintain these, I just wanted *a* way to download a story the first time, and then got a request to download a different story off a different website and was like, surely this can't be too different. It was, unfortunately, but there was commonality and each time it gets a little easier/faster to modify the script to do the specific thing I want it to do and then ignore it until I need it again, probably for yet another website.

### `epub-ify.py`

```
python3 epub-ify.py folder-name/ "Title of story" "Author name"
```

This should spit out an epub file based on the chapters in the folder. It'll include the chapter range in the epub file name (e.g. 1-42). I think this is because a previous version of this script was designed to make epubs in batches if there were too many chapters. I think I modified the script at least a little, but I definitely got the base code from somewhere and I don't remember where unfortunately.