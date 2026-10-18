// ==UserScript==
// @name         AO3 Bookmark info autofill
// @version      1.0
// @description  Automatically add work info to the ao3 bookmark form whenever it is opened.
// @author       irrationalpie7
// @match        https://archiveofourown.org/*
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/bookmark-fic-info.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/bookmark-fic-info.pub.user.js
// @require      http://code.jquery.com/jquery-3.6.0.min.js
// @grant        none
// ==/UserScript==

(async () => {
  "use strict";

  /* bookmarklet start below */
  /* javascript: (() => { */

  /* following two are similar, check first *then* second if failed first */
  const work_bookmarks_url =
    /https:\/\/archiveofourown\.org\/works\/\d+\/bookmarks/;
  const work_url =
    /https:\/\/archiveofourown\.org\/(collections\/[^\/]*\/)?works\/\d+/;
  const series_url = /https:\/\/archiveofourown\.org\/series\/\d+/;
  const bookmark_form_url =
    /https:\/\/archiveofourown\.org\/bookmarks\/\d+\/edit/;

  const seen = new Set();

  /**
   * Populate the work info
   * @param {HTMLElement | null} bookmarkForm
   * @returns {undefined}
   */
  function autopopulateWorkInfo(bookmarkForm) {
    const form = bookmarkForm?.querySelector("form");
    /* Make sure it's a bookmark form, and extract an id */
    const bookmarkId =
      /(\/bookmarks\/\d+$)|(\/\d+\/bookmarks$)/.exec(form?.action ?? "")?.[0] ??
      "";
    if (!bookmarkId) {
      return;
    }
    /* check if we've looked at this form yet since loading the page */
    if (seen.includes(bookmarkId)) {
      return;
    }
    seen.add(bookmarkId);

    const href = window.location.href;
    if (href.match(bookmark_form_url)) {
      /* Dedicated bookmark edit page, work info not accessable */
      return;
    }

    /* what page is this? can we find the work info? */
    let title = null;
    let titleText = null;
    let authors = null;
    if (!href.match(work_bookmarks_url) && href.match(work_url)) {
      titleText = document.querySelector(".title.heading").textContent.trim();
      title = `<a href="${href}">${titleText}</a>`;
      authors = getAuthors(
        document.querySelector(".byline.heading"),
        'a[rel="author"]',
      );
    } else if (href.match(series_url)) {
      titleText = document.querySelector("#main h2.heading").textContent.trim();
      title = `<a href=${href}>${titleText}</a>`;
      authors = getAuthors(document, '.series.meta.group a[rel="author"]');
    } else {
      /* Most other pages have the bookmark form nested within a bookmark article */
      let bookmarkArticle = bookmarkForm.closest("li.bookmark[role=article]");
      /* If that didn't work, but there's exactly one bookmark on the page, fall back to that. */
      if (bookmarkArticle === null) {
        const bookmarks = document.querySelectorAll(
          "li.work[role=article], li.series[role=article]",
        );
        if (bookmarks.length !== 1) {
          /* Still couldn't find the bookmark article, so give up */
          return;
        }
        bookmarkArticle = bookmarks[0];
      }

      const titleElement = bookmarkArticle.querySelector(
        'h4.heading a[href*="/works/"], h4.heading a[href*="/series/"]',
      );
      titleText = titleElement.textContent;
      title = titleElement.outerHTML;
      authors = getAuthors(bookmarkArticle, 'h4.heading a[rel="author"]');
    }

    const workInfo = `${title}\nby ${authors}`;

    const textArea = bookmarkForm.querySelector("textarea");

    if (!textArea.value.includes(titleText + "</a>")) {
      if (textArea.value === "") {
        textArea.value = workInfo;
      } else {
        textArea.value += "\n\n" + workInfo;
      }
    }
  }

  function getAuthors(parent, authorSelector) {
    const authors = parent.querySelectorAll(authorSelector);
    if (authors.length === 0) {
      return "Anonymous";
    }
    return Array.from(authors)
      .map((a) => a.outerHTML)
      .join(", ");
  }

  /* autopopulateWorkInfo(document.querySelector("#bookmark-form"));})(); */
  /* bookmarklet end above */

  /**
   * Wait for certain elements to appear.
   * @param {string} selectorTxt
   * @param {(element: HTMLElement) => undefined} actionFunction The code to
   *    run when elements are found. Note: it's up to actionFunction to ignore
   *    repeated calls after the elements are already found.
   * @param {boolean | undefined} bWaitOnce If false, will continue to scan for
   *    new elements even after the first match is
   *    found.
   */
  function waitForKeyElements(selectorTxt, actionFunction, bWaitOnce) {
    /** @type{Element[]} */
    const targetNodes = Array.from(document.querySelectorAll(selectorTxt));
    targetNodes.forEach((node) => actionFunction(node));

    //--- Get the timer-control variable for this selector.
    var controlObj = waitForKeyElements.controlObj || {};
    var controlKey = selectorTxt.replace(/[^\w]/g, "_");
    var timeControl = controlObj[controlKey];

    //--- Now set or clear the timer as appropriate.
    if (targetNodes.length > 0 && bWaitOnce && timeControl) {
      //--- The only condition where we need to clear the timer.
      clearInterval(timeControl);
      delete controlObj[controlKey];
    } else {
      //--- Set a timer, if needed.
      if (!timeControl) {
        timeControl = setInterval(function () {
          waitForKeyElements(selector, actionFunction, bWaitOnce);
        }, 300);
        controlObj[controlKey] = timeControl;
      }
    }
    waitForKeyElements.controlObj = controlObj;
  }

  // $("[id='idofelement']") due to multiple elements with same id being possible on ao3 bookmark page
  waitForKeyElements(
    "[id='bookmark-form']",
    autopopulateWorkInfo,
    false /*false = continue searching after first*/,
  );
})();
