from bs4 import BeautifulSoup
import glob
import sys
import re
import os
import requests
from pathlib import Path
import asyncio
from playwright.async_api import async_playwright, TimeoutError as PlaywrightTimeoutError


async def playwright_main(urls, dir, wait_selector, offset):
    # Use 'async with' to manage the playwright instance
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True)

        failure_count = 0
        success_count = 0
        for i, url in enumerate(urls[offset:], start=offset):
            success = await save_scripted_content(url, i, dir, wait_selector, browser)
            if success:
                success_count += 1
            if not success:
                if i == offset:
                    print(
                        f"Failed to retrieve first attempted url ({url}). If this url looks correct, your IP has likely been banned. Check the associated error file (error-file-{i}.html) to verify. Aborting!")
                    return
                failure_count += 1
                print(
                    f"Failure #{failure_count}. Will sleep to try to avoid rate-limiting")
                # Sleeps for failure_count minutes
                await asyncio.sleep(failure_count * 60)
                print("Done sleeping!")

        print(
            f"Successfully retrieved {success_count} chapters and skipped {failure_count} chapters. Rerun the script with outdir={dir} to retrieve missing chapters!")

        # This method is probably more prone to rate-limiting:
        # await asyncio.gather(*[save_scripted_content(url, i, dir, wait_selector, browser) for i, url in enumerate(urls[offset:], start=offset)])

        # Close the browser
        await browser.close()


async def save_scripted_content(url, i, dir, wait_selector, browser):
    path = Path(dir, f"file-{i}.html")
    if os.path.exists(path):
        # print(f" - file-{i}.html already exists; skipping current url")
        return

    page = await browser.new_page()
    await page.goto(url)

    # Wait for the script-loaded element to appear
    save = True
    try:
        await page.wait_for_selector(wait_selector)
    except PlaywrightTimeoutError:
        print(
            f"Unable to download chapter at: {url}. Likely, we are being rate-limited or the site has otherwise detected suspicious activity. Check error-file-{i}.html, and try rerunning the script to get missing chapters.")
        save = False

    if save:
        with open(path, "w", newline="", encoding="utf-8") as file:
            file.write(await page.content())
    else:
        with open(Path(dir, f"error-file-{i}.html"), "w", newline="", encoding="utf-8") as file:
            file.write(await page.content())


def create_unique_dir(file_dir):
    """
    Example: file_dir = "raw-html"

    This function finds the first of raw-html-1, raw-html-2 etc that
    doesn't exist yet, creates that dir, and returns the name
    """
    i = 1
    while os.path.exists(f"{file_dir}-{i}"):
        i += 1
    file_dir = f"{file_dir}-{i}"
    os.mkdir(file_dir)
    return file_dir


def save_html(dir, i, url, matcher=None):
    path = Path(dir, f"file-{i}.html")
    if os.path.exists(path):
        # print(f" - file-{i}.html already exists; skipping current url")
        return

    response = requests.get(url)
    if response.status_code == 200:
        with open(path, "w", newline="", encoding="utf-8") as file:
            file.write(response.text)
        if matcher is not None:
            soup = BeautifulSoup(response.text, 'html.parser')
            return extract_urls_from_soup(url, soup, matcher)
        else:
            return []
    else:
        print(f"Skipping {path}, could not reach url: {url}")


def atoi(text):
    return int(text) if text.isdigit() else text


def natural_keys(text):
    '''
    alist.sort(key=natural_keys) sorts in human order
    http://nedbatchelder.com/blog/200712/human_sorting.html
    (See Toothy's implementation in the comments)
    '''
    return [atoi(c) for c in re.split(r'(\d+)', text)]


def extract_urls_from_soup(orig_url, soup, matcher):
    return [canonical(orig_url, x["href"]) for x in soup.select(matcher)]


def extract_urls_from_file(orig_url, filename, matcher):
    with open(filename) as fp:
        soup = BeautifulSoup(fp, 'html.parser')
    return extract_urls_from_soup(orig_url, soup, matcher)


def extract_urls(orig_url, folder, matcher):
    if not folder.endswith("/"):
        folder += "/"
    files = [file for file in glob.glob(f"{folder}*.html")]
    files.sort(key=natural_keys)
    url_list_list = [extract_urls_from_file(orig_url, filename, matcher)
                     for filename in files]
    return [url for sublist in url_list_list for url in sublist]


def canonical(orig_url: str, relative_url: str) -> str:
    m = re.search(r'https?://.*', relative_url)
    if m:
        return relative_url
    m = re.search(r'https?://[^/]*', orig_url)
    # pyright: ignore[reportOptionalMemberAccess]
    return f"{m.group(0)}{relative_url}"


def wait_selector_by_host(url: str):
    if "wtr-lab.com" in url:
        return ".pr-line-text"

    return None


def extract_urls_by_host(url: str, num: int, has_in_dir: bool, in_dir: str | None) -> list[str]:

    # format to add new host site:
    # if "SITENAME" in url
    #     # if we're parsing files to get the TOC
    #     if has_in_dir:
    #         return extract_urls(url, in_dir, "TOC_CHAPTER_SELECTOR")
    #     # if we're not, how do we turn this url into the url or set of urls
    #     # that we need to fetch from?
    #     return [args["url"]] OR list comprehension

    if "www.wuxiaworld.eu" in url:
        # Example url: https://www.wuxiaworld.eu/novel/the-e-sports-circles-toxic-assembly-camp
        # num=216
        url = url.replace('/novel/', '/chapter/')
        return [
            f"{url}-{x}" for x in range(1, num+1)]

    if "novelfull.com" in url:
        # Example url: https://novelfull.com/reincarnation-of-the-strongest-sword-god.html
        if has_in_dir:
            return extract_urls(
                url, in_dir, ".list-chapter a")
        # multi-page TOC
        return [f"{url}?page={x}" for x in range(1, num+1)]

    if "tumblr" in url:
        if has_in_dir:
            # include the url (the TOC) as the last chapter
            return extract_urls(
                url, in_dir, ".captext a") + [url]
        # single-page TOC
        return [url]

    if "novelingua" in url:
        if has_in_dir:
            return extract_urls(
                url, in_dir, "#main a")
        # single-page TOC
        return [url]

    # https://wtr-lab.com/en/novel/19458/game-invasion-starting-with-a-random-system-draw/chapter-301?service=web
    if "wtr-lab.com" in url:
        # note: I think this method will fail, so don't specify an in_dir
        if has_in_dir:
            return extract_urls(
                url, in_dir, "a.chapter-item")
        # multiple URLs
        return [f"{url}/chapter-{x}?service=web" for x in range(1, num+1)]

    raise Exception(
        "Unsupported url host; please add it to the parse_url function")


def parse_args():
    print('\nUsage: python webnovel-downloader.py <url>\nOptional arguments:\n' +
          " - max=<num> : the maximum page to download (defaults to 1, okay to overshoot)\n" + " - offset=<num> : the number of pages to skip (defaults to 0)\n" +
          " - in_dir=<folder> : the folder of downloaded table of contents page(s), if any\n" +
          " - out_dir=<folder> : the folder to save downloaded pages in\n" + "           (by default, a new folder is created)\n" + "           (note: if this folder already has files, and a file would be\n" + "           overwritten by a new download, it will be skipped instead)\n" + "Do a trial run first--out_dir is required if max > 1")

    # find the first url in the list
    t = [x for x in sys.argv if x.startswith("http")]
    if len(t) == 0:
        raise Exception(
            "Please provide a url argument, even when pulling from TOC files")
    url = t[0]
    print(f"Fetching novel from: {url}")

    # find the number of things to fetch (defaults to 1)
    num = 1
    t = [x[4:] for x in sys.argv if x.startswith("max=")]
    if len(t) > 0:
        num = int(t[0])

    # find the offset to start at (defaults to 0)
    offset = 0
    t = [x[7:] for x in sys.argv if x.startswith("offset=")]
    if len(t) > 0:
        offset = int(t[0])

    has_in_dir = False
    in_dir = None
    # dir to pull from, instead of or in addition to web requests
    t = [x[7:] for x in sys.argv if x.startswith("in_dir=")]
    if len(t) > 0:
        in_dir = t[0]
        print(f" - Parsing TOC from: {in_dir}")
        has_in_dir = True

    # dir to save to
    t = [x[8:] for x in sys.argv if x.startswith("out_dir=")]
    if len(t) > 0:
        out_dir = t[0]
    else:
        if num > 1:
            raise Exception("out_dir is required if max is specified")
        out_dir = create_unique_dir("raw-html")
    print(f" - Saving to: {out_dir}")

    # arrange result
    return {"url": url, "num": num, "offset": offset, "out_dir": out_dir,
            "url_list": extract_urls_by_host(url, num, has_in_dir, in_dir)}


def main():

    args = parse_args()

    num = min(len(args["url_list"]), args["num"])
    offset = args["offset"]
    wait_selector = wait_selector_by_host(args["url_list"][0])
    print(f"Saving from {offset} to {num}")

    if wait_selector is not None:
        asyncio.run(playwright_main(
            args["url_list"], args["out_dir"], wait_selector, offset))
        return

    for i in range(args["offset"], num):
        url = args["url_list"][i]
        save_html(args["out_dir"], i, url, None)
        if ((i - args["offset"] + 1) % 50 == 0):
            print(
                f"... just saved #{i + 1} of {num} (progress is {i - offset + 1} of {num - offset})")


if __name__ == "__main__":
    main()
