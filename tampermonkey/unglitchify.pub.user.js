// ==UserScript==
// @name        Unglitchify text
// @namespace   irrationalpie scripts
// @match       https://archiveofourown.org/*
// @grant       none
// @version     1.1
// @author      irrationalpie
// @updateURL   https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/unglitchify.pub.user.js
// @downloadURL https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/unglitchify.pub.user.js
// @description Remove glitchy characters from text
// ==/UserScript==

/** 
 * Removes glitchy characters from text.
 *
 * Modified from https://cable.ayra.ch/zalgo/
 *
 * @param {string} origText
 * @returns {string} text with glitchy characters removed
 */
function unzalgo (origText) {
  // Excluded sequence is 0300-036F: Combining Diacritical Marks
	const r = /[^\u0300-\u036F\u0489]+/g;
  const matches = origText.match(r) || [""];
  var result = matches.join("");
  if (result !== origText) {
    var prevLong = true;
    result = "";
    for (const match of matches) {
      if (prevLong && match.length === 1) {
        result += "[[";
      }
      if (!prevLong && match.length > 1) {
        result += "]]";
      }
      result += match;
      prevLong = match.length > 1;
    }
    if (!prevLong) {
      result += "]]";
    }
  }
  return result;
}

/**
 * Cleans text in all children of the given element.
 *
 * @param {HTMLElement} element
 */
function recursiveClean(element) {
  // Snapshot the children to iterate over them without having to worry about
  // whether the list changes as we go along.
  const currentChildren = Array.from(element.childNodes);
  for (const child of currentChildren) {
    if (child.nodeType === Node.TEXT_NODE) {
      const newText = unzalgo(child.textContent);
      if (newText !== child.textContent) {
        child.textContent = newText;
      }
    } else {
      recursiveClean(child);
    }
  }
}

console.log("cleaning glitchy text");
recursiveClean(document.body);
