// ==UserScript==
// @name         AO3 all-chapter word counts
// @version      1.1
// @description  Add chapter word counts on a work's full-page index
// @author       irrationalpie7
// @match        https://archiveofourown.org/works/*/navigate*
// clang-format off
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/all-word-counts.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/all-word-counts.pub.user.js
// @require      https://github.com/irrationalpie7/words-count/raw/master/src/globalWordsCount.js
// clang-format on
// ==/UserScript==

(async () => {
  'use strict';

  async function insertChapterWordCounts() {
    // Parse current url to make sure we're on the navigate page.
    const url = window.location.href;
    const navigatePageRegex =
        new RegExp('^https://archiveofourown[.]org(/.*)?/works/[0-9]+/navigate')
    if (!navigatePageRegex.test(url)) {
      return;
    }
    const workId =
        parseInt(String(url.match(/\/works\/[0-9]+/)).match(/[0-9]+/));

    // Retrieve chapter word counts from full-page work
    console.log('Attempting to retrieve chapter word counts')
    const chapters =
        Array.from(window.document.querySelectorAll('ol.chapter > li'))
    const chapterLengths = await importChapterCounts(workId, chapters.length);

    // Insert chapter word counts on current page.
    chapters.forEach((chapterEl, i) => {
      if (chapterLengths[i] >= 0) {
        const wordCountEl =
            window.document.createElement('span');
        wordCountEl.classList.add('chapter-words');
        wordCountEl.textContent = '(Words: ' +
            chapterLengths[i].toLocaleString(
                (navigator.languages && navigator.languages.length) ?
                    navigator.languages[0] :
                    navigator.language) +
            ')';
        chapterEl.append(' ');
        chapterEl.append(wordCountEl);
      } else {
        console.log('Unable to retrieve word count for chapter ' + (i + 1));
      }
    });
  }
  insertChapterWordCounts();

  /**
   * Import chapter lengths for the work associated with this work id.
   * @param {int} workId
   * @param {int} numChapters
   * @returns {Promise<int[]>} chapter lengths
   */
  async function importChapterCounts(workId, numChapters) {
    // Attempt to parse the URL
    /** @type {URL} */
    let fetchUrl = new URL('https://archiveofourown.org/works/' + workId);
    // Always fetch the full work, and consent to seeing adult content to make
    // sure we actually receive it.
    fetchUrl.searchParams.set('view_adult', 'true');
    fetchUrl.searchParams.set('view_full_work', 'true');

    let result = await fetch(fetchUrl, {credentials: 'include'});
    const html = await result.text();
    const domParser = new DOMParser();
    const doc = domParser.parseFromString(html, 'text/html');
    return Array.from(doc.querySelectorAll('#chapters .chapter'))
        .filter(chapter => /^chapter-[0-9]+$/.test(chapter.id))
        .reduce((total, chapter) => {
          const chapterNum = parseInt(chapter.id.match(/[0-9]+/))
          if (chapterNum >= 1 && chapterNum <= numChapters) {
            total[chapterNum - 1] = wordsCount(
                chapter.querySelector(':scope > .userstuff').innerText);
          }
          return total;
        }, Array.from({length: numChapters}, _ => -1));
  }
})();
