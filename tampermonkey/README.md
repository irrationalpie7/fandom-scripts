# Miscellaneous tampermonkey/greasemonkey extensions

To use any of these, you will first need to install [Tampermonkey](https://www.tampermonkey.net/) or [Greasemonkey](https://www.greasespot.net/). They are intended to be usable on both Firefox and Chrome, but I may not test usage on Firefox as thoroughly. Please feel free to file an issue here or contact me by gmail (also irrationalpie7) if you notice any issues or have ideas for improvements!

## AO3 all-chapter word counts
You can [install the all-chapter word counts](https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/all-word-counts.pub.user.js) script here. Enjoy!

### Usage

From a multi-chapter work, use the chapter-dropdown ("Chapter Index") but hit "Full-page index" instead of picking a chapter. This will take you to a page with a url something like `https://archiveofourown.org/works/<work id>/navigate`, where `<work id>` is a bunch of numbers.

On that page, you would normally see something like this, where each chapter links to the individual chapter, and the date is when the chapter was published:

> 1. [Chapter 1](#) (2022-05-28)
> 2. [Chapter 2](#) (2022-06-02)

After installing the extension, it will instead look something like this, where the end of each line is each chapter's individual word count:

> 1. [Chapter 1](#) (2022-05-28) (Words: 3,370)
> 2. [Chapter 2](#) (2022-06-02) (Words: 2,807)

The script may take a moment to load, since it has to fetch the content of the full-work page so that it can count the number of words in each chapter. This will not necessarily match AO3's word counts precisely. I'm using [this word counting implementation](https://github.com/byn9826/words-count), though the version I'm using may not be 100% up-to-date.

## Tone Marks II

This extension adds pinyin accent marks to work/series pages and blurbs on AO3 for some fandoms. More info [here](https://github.com/irrationalpie7/AO3-Tone-Marks).