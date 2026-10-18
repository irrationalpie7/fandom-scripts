// ==UserScript==
// @name         AO3 Bookmark info autofill
// @version      1.2
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

  /* scan for bookmark form(s) */
  scanForElements();

  /* set things up so that any time a bookmark form appears, we re-scan */
  const formTriggers = Array.from(
    document.querySelectorAll('[class*="bookmark_form_trigger"]'),
  );
  formTriggers.forEach((trigger) => {
    trigger.addEventListener("click", () =>
      /* Wait for the form to load after the click before scanning */
      setTimeout(() => scanForElements(), 500),
    );
  });

  /* following two are similar, check first *then* second if failed first */
  const work_bookmarks_url =
    /https:\/\/archiveofourown\.org\/works\/\d+\/bookmarks/;
  const work_url =
    /https:\/\/archiveofourown\.org\/(collections\/[^\/]*\/)?works\/\d+/;
  const series_url = /https:\/\/archiveofourown\.org\/series\/\d+/;
  const bookmark_form_url =
    /https:\/\/archiveofourown\.org\/bookmarks\/\d+\/edit/;
  /** forms we've already previously detected in the page */
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
    if (seen.has(bookmarkId)) {
      return;
    }
    seen.add(bookmarkId);

    const href = window.location.href;
    if (href.match(bookmark_form_url)) {
      /* Dedicated bookmark edit page, work info not accessable */
      return;
    }

    /* what page is this? can we find the work info? */
    const workInfoObj = getWorkInfo(href, bookmarkForm);
    if (workInfoObj === null) {
      return;
    }
    const { title, titleText, authors } = workInfoObj;

    const workInfo = `${title}\nby ${authors}`;

    const textArea = bookmarkForm.querySelector("textarea");

    /* Return early if the textarea already has a link to the title */
    if (textArea.value.includes(`>${titleText}</a>`)) {
      return;
    }
    if (textArea.value === "") {
      textArea.value = workInfo;
    } else {
      textArea.value += "\n\n" + workInfo;
    }
  }

  /**
   * Get work info
   * @param {string} href The page we're on
   * @param {HTMLElement} bookmarkForm
   * @returns {{titleText: string,title: string, authors: string}| null }
   */
  function getWorkInfo(href, bookmarkForm) {
    /* what page is this? can we find the work info? */
    if (!href.match(work_bookmarks_url) && href.match(work_url)) {
      const titleText = document
        .querySelector(".title.heading")
        .textContent.trim();
      return {
        titleText,
        title: `<a href="${href}">${titleText}</a>`,
        authors: getAuthors(
          document.querySelector(".byline.heading"),
          'a[rel="author"]',
        ),
      };
    }
    if (href.match(series_url)) {
      const titleText = document
        .querySelector("#main h2.heading")
        .textContent.trim();
      return {
        titleText,
        title: `<a href=${href}>${titleText}</a>`,
        authors: getAuthors(document, '.series.meta.group a[rel="author"]'),
      };
    }
    /* Most other pages have the bookmark form nested within a bookmark article */
    let bookmarkArticle = bookmarkForm.closest("li.bookmark[role=article]");
    /* If that didn't work, but there's exactly one bookmark on the page, fall back to that. */
    if (bookmarkArticle === null) {
      const bookmarks = document.querySelectorAll(
        "li.work[role=article], li.series[role=article]",
      );
      if (bookmarks.length !== 1) {
        /* Still couldn't find the bookmark article, so give up */
        return null;
      }
      bookmarkArticle = bookmarks[0];
    }

    const titleElement = bookmarkArticle.querySelector(
      'h4.heading a[href*="/works/"], h4.heading a[href*="/series/"]',
    );
    return {
      titleText: titleElement.textContent,
      title: titleElement.outerHTML,
      authors: getAuthors(bookmarkArticle, 'h4.heading a[rel="author"]'),
    };
  }

  /**
   * Get html element (as string) representation of work author(s)
   *
   * @param {HTMLElement} parent
   * @param {string} authorSelector
   * @returns {string}
   */
  function getAuthors(parent, authorSelector) {
    const authors = parent.querySelectorAll(authorSelector);
    if (authors.length === 0) {
      return "Anonymous";
    }
    return Array.from(authors)
      .map((a) => a.outerHTML)
      .join(", ");
  }

  function scanForElements() {
    /* $("[id='idofelement']") due to multiple elements with same id being possible on ao3 bookmark page */
    const targetNodes = Array.from(
      document.querySelectorAll("[id='bookmark-form']"),
    );
    targetNodes.forEach((node) => autopopulateWorkInfo(node));
  }

  /* })(); */
  /* bookmarklet end above */
})();
