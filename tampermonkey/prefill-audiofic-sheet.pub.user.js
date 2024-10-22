// ==UserScript==
// @name         Prefill audiofic archive spreadsheet
// @version      1.0
// @description  Automatically generate a spreadsheet from a list of works
// @author       irrationalpie7
// @match        https://archiveofourown.org/*
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/prefill-audiofic-sheet.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/prefill-audiofic-sheet.pub.user.js
// @grant        none
// ==/UserScript==

/* For use as bookmarklet, delete this line and all previous lines. */

/* javascript: (() => { /**/

function generateSpreadsheet() {
  const blurbs = Array.from(document.querySelectorAll(".blurb"));
  const works = blurbs.map((blurb) => parseBlurb(blurb));
  console.log(works);
  console.log(works[0]);

  const spreadsheetHeader =
    "*Note: author and text links are merely guesses based on links present in the work summary. Please check that these were detected correctly!\nPodficcer Name(s) (the reader(s) of this podfic)\tWhere should podfic feedback be directed?\tPodfic Title\tFandom(s)\tRelationship(s) (if there are no romantic relationships, just put \"gen\")\tLength of Recording (HH:MM:SS)\tFic author name(s)\tURL of text version (if it's publically available)\tCover Artist\tLanguage\tIs this work part of a series? (give series name/link)\tNotes (eg. tell us if a podfic shouldn't be published until a challenge goes live, etc.)\n";

  copyToClipboard(
    spreadsheetHeader + works.map((w) => printWork(w)).join("\n")
  );
}

function copyToClipboard(text) {
  const copyText = document.createElement("textarea");
  copyText.value = text;
  copyText.classList.add("hidden");
  main.append(copyText);
  copyText.select();
  copyText.setSelectionRange(0, 99999); /* For mobile devices */
  navigator.clipboard.writeText(copyText.value);
}

function parseBlurb(blurb) {
  const work = {};
  work.podficcers = Array.from(blurb.querySelectorAll("h4 a[rel='author']"))
    .map((a) => a.textContent)
    .join(", ");
  work.url = blurb.querySelector("h4 a").href;
  work.title = blurb.querySelector("h4 a").textContent;
  work.fandoms = Array.from(blurb.querySelectorAll("h5.fandoms a"))
    .map((a) => a.textContent)
    .join(", ");
  work.relationships = Array.from(blurb.querySelectorAll("li.relationships a"))
    .map((a) => a.textContent)
    .filter((a) => a.includes("/"))
    .join(", ");
  if (work.relationships === "") {
    work.relationships = "gen";
  }
  work.language = blurb.querySelector("dd.language").textContent;
  const links = Array.from(blurb.querySelectorAll("blockquote.summary a"));
  work.authors = links
    .filter((a) => a.href.includes("/users/"))
    .map((a) => a.textContent)
    .join(", ");
  work.texts = links
    .filter((a) => a.href.includes("/works/"))
    .map((a) => a.href)
    .join(" ; ");
  work.links = links
    .filter((a) => !a.href.includes("/users/") && !a.href.includes("/works/"))
    .map((a) => `${a.textContent} (${a.href})`)
    .join(" ; ");
  work.series = Array.from(blurb.querySelectorAll("ul.series li"))
    .map((li) => {
      const a = li.querySelector("a");
      const num = li.querySelector("strong");
      return `${a.textContent} [#${num.textContent}] (${a.href})`;
    })
    .join(", ");
  return work;
}

function printWork(work) {
  return `${work.podficcers}\t${work.url}\t${work.title}\t${work.fandoms}\t${work.relationships}\t\t${work.authors}\t${work.texts}\t\t${work.language}\t${work.series}\t${work.links}`.replaceAll(
    "\n",
    ""
  );
}

/* generateSpreadsheet();})(); /**/

/* For use as bookmarklet, delete this line and all subsequent lines. */

function generateGenerateButton() {
  const generateButton = document.createElement("button");
  generateButton.textContent =
    "Generate and copy audiofic archive spreadsheet starter";
  generateButton.addEventListener("click", generateSpreadsheet);
  document.querySelector("#main").append(generateButton);
}

if (document.querySelectorAll(".blurb").length !== 0) {
  generateGenerateButton();
}
