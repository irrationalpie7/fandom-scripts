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

## Unglitchify text

The [Unglitchify text user script (install)](https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/unglitchify.pub.user.js) cleans up glitchy characters from text on ao3. This is specifically referring to text like "P̶̳̘̗̚͜r̸̬̤͝o̷̱̖̰̐̽̀t̴͍̲̦̞̀́͝e̷̹̥̭̦͌c̶̘̣̲͂̒̐͝ͅt̶̥͎̅" ("Protect"). The implementation is inspired by this [other online tool to remove glitchy text](https://cable.ayra.ch/zalgo/).

### Limitations

1. If you find that after you install this, useful diacritic marks are missing on a particular story, I recommend temporarily disabling this userscript and reloading the page.
2. This user script will run on all ao3 pages when it is enabled, and particularly for long stories the page might freeze while this runs, so it may make sense to keep this particular user script disabled except when you come across a story which requires it.
3. This user script does not leave any indication that it has modified the page, so if the presence/absence of glitchy characters was being used to encode some sort of meaning, you will miss out on that.

## Fix AO3 text editing
You can [install the Fix AO3 text editing](https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/fix-ao3-text-editing.pub.user.js) script here. It attempts to fix ao3 text boxes on the edit work page when ao3 messes up your spacing. Current changes:

1. Empty paragraph tags (or those with only spaces inside) will be replaced with a regular new line
2. Any more than two empty lines in a row will be reduced to just two new lines (lines with just spaces count as empty for this purpose)
3. Certain special characters (non-breaking space, word join) will be replaced with their html codes (`&nbsp;`, etc), because otherwise AO3 will strip them when you post (at least, that was the behavior I thought I observed as of Oct 13, '23).

Feel free to let me know if you have suggestions for improvement!

## Smooth scrolling

This extension slowly scrolls ao3 as you read. Use ctrl+x to scroll slowly, then slightly faster, then turn scrolling back off. I've tried to use comments to indicate how you could customize the shortcut and/or scrolling speeds if you have different preferences. [Install smooth scrolling](https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/smooth-scrolling.pub.user.js).

## Tone Marks II

This extension adds pinyin accent marks to work/series pages and blurbs on AO3 for some fandoms. More info [here](https://github.com/irrationalpie7/AO3-Tone-Marks).
