// ==UserScript==
// @name         AO3 Bookmark info autofill
// @version      0.1
// @description  Automatically add work info to the ao3 bookmark form whenever it is opened.
// @author       irrationalpie7
// @match        https://archiveofourown.org/*
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/bookmark-fic-info.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/bookmark-fic-info.pub.user.js
// @require      http://code.jquery.com/jquery-3.6.0.min.js
// @grant        none
// ==/UserScript==

// following two are similar, check first *then* second if failed first
const work_bookmarks_url =
  /https:\/\/archiveofourown\.org\/works\/\d+\/bookmarks/;
const work_url =
  /https:\/\/archiveofourown\.org\/(collections\/[^\/]*\/)?works\/\d+/;
const series_url = /https:\/\/archiveofourown\.org\/series\/\d+/;
const bookmark_form_url =
  /https:\/\/archiveofourown\.org\/bookmarks\/\d+\/edit/;

function autopopulate_wordcount() {
  if (this === window) {
    return; // Don't know why, but the id selector also returns window?
  }
  if (href.match(bookmark_form_url)) {
    // Dedicated bookmark edit page, work info not accessable
    return;
  }

  let dds = $(this).find("fieldset > fieldset > dl > dd");
  let tag_dd = dds[1];
  let tag_list = $(tag_dd).children("ul");

  let tag_input = $(this).find("[id=bookmark_tag_string_autocomplete]");
  tag_input = tag_input[0]; // get actual DOM node

  // what page is this? can we find the work info?
  let href = window.location.href;
  let title = null;
  let authors = null;
  if (!href.match(work_bookmarks_url) && href.match(work_url)) {
    title = `<a href="${href}">${$(".title.heading")[0].textContent}</a>`;
    authors = getAuthors($(".byline.heading")[0].find('a[rel="author"]'));
  } else if (href.match(series_url)) {
    title = `<a href=${href}>${$(
      "#main h2.heading"
    )[0].textContent.trim()}</a>`;
    authors = getAuthors($('.series.meta.group a[rel="author"]'));
  } else {
    // All other pages have the bookmark form nested within a bookmark article
    let bookmark_article = $(this).closest("li.bookmark[role=article]");

    title = $(
      'h4.heading a[href*="/works/"], h4.heading a[href*="/series/"]',
      bookmark_article
    )[0].outerHTML;
    authors = getAuthors($('h4.heading a[rel="author"]', bookmark_article));
  }

  let workInfo = `${title}\nby ${authors}`;

  let textArea = $(this).find("textarea")[0];

  if (!textArea.value.includes(workInfo)) {
    if (textArea.value === "") {
      textArea.value = workInfo;
    } else {
      textArea.value += "\n\n" + workInfo;
    }
  }
}

function getAuthors(authors) {
  if (authors.length === 0) {
    return "Anonymous";
  }
  return Array.from(authors)
    .map((a) => a.outerHTML)
    .join();
}

function set_context(j_node) {
  autopopulate_wordcount.call(j_node);
}

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
      var alreadyFound = jThis.data("alreadyFound") || false;

      if (!alreadyFound) {
        //--- Call the payload function.
        var cancelFound = actionFunction(jThis);
        if (cancelFound) btargetsFound = false;
        else jThis.data("alreadyFound", true);
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
  set_context,
  false /*false = continue searching after first*/
);
