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

/* javascript: (() => { */

/* following two are similar, check first *then* second if failed first */
const work_bookmarks_url =
  /https:\/\/archiveofourown\.org\/works\/\d+\/bookmarks/;
const work_url =
  /https:\/\/archiveofourown\.org\/(collections\/[^\/]*\/)?works\/\d+/;
const series_url = /https:\/\/archiveofourown\.org\/series\/\d+/;
const bookmark_form_url =
  /https:\/\/archiveofourown\.org\/bookmarks\/\d+\/edit/;

function autopopulateWorkInfo(bookmarkForm) {
  if (bookmarkForm === window) {
    return; /* Don't know why, but the id selector also returns window? */
  }
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
      'a[rel="author"]'
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
        "li.work[role=article], li.series[role=article]"
      );
      if (bookmarks.length !== 1) {
        /* Still couldn't find the bookmark article, so give up */
        return;
      }
      bookmarkArticle = bookmarks[0];
    }

    const titleElement = bookmarkArticle.querySelector(
      'h4.heading a[href*="/works/"], h4.heading a[href*="/series/"]'
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

function waitForKeyElements(
  selectorTxt /* Required: The jQuery selector string that
                        specifies the desired element(s).
                    */,
  actionFunction /* Required: The code to run when elements are
                        found. It is passed a jNode to the matched
                        element.
                    */,
  bWaitOnce /* Optional: If false, will continue to scan for
                        new elements even after the first match is
                        found.
                    */,
  iframeSelector /* Optional: If set, identifies the iframe to
                        search.
                    */
) {
  var targetNodes, btargetsFound;

  if (typeof iframeSelector == "undefined") targetNodes = $(selectorTxt);
  else targetNodes = $(iframeSelector).contents().find(selectorTxt);

  if (targetNodes && targetNodes.length > 0) {
    btargetsFound = true;
    /*--- Found target node(s).  Go through each and act if they
            are new.
        */
    targetNodes.each(function () {
      var jThis = $(this);
      var alreadyFound = jThis.data("alreadyFound2") || false;

      if (!alreadyFound) {
        //--- Call the payload function.
        var cancelFound = actionFunction(jThis[0]);
        if (cancelFound) btargetsFound = false;
        else jThis.data("alreadyFound2", true);
      }
    });
  } else {
    btargetsFound = false;
  }

  //--- Get the timer-control variable for this selector.
  var controlObj = waitForKeyElements.controlObj || {};
  var controlKey = selectorTxt.replace(/[^\w]/g, "_");
  var timeControl = controlObj[controlKey];

  //--- Now set or clear the timer as appropriate.
  if (btargetsFound && bWaitOnce && timeControl) {
    //--- The only condition where we need to clear the timer.
    clearInterval(timeControl);
    delete controlObj[controlKey];
  } else {
    //--- Set a timer, if needed.
    if (!timeControl) {
      timeControl = setInterval(function () {
        waitForKeyElements(
          selectorTxt,
          actionFunction,
          bWaitOnce,
          iframeSelector
        );
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
  false /*false = continue searching after first*/
);
