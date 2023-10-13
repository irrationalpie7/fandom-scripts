// ==UserScript==
// @name        Fix AO3 text editing
// @namespace   irrationalpie scripts
// @match       https://archiveofourown.org/works/*
// @grant       none
// @version     1.2
// @author      irrationalpie
// @updateURL   https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/fix-ao3-text-editing.pub.user.js
// @downloadURL https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/fix-ao3-text-editing.pub.user.js
// @description Attempt to fix AO3 changing text spacing when you edit
// ==/UserScript==

doTheThing();

function doTheThing() {
  // only operate on new work or edit work page
  const main = document.querySelector(
    "#main.works-new, #main.works-edit, #main.chapters-edit"
  );
  if (main === null) {
    return;
  }

  /** @type {HTMLTextAreaElement[]} */
  const textareas = Array.from(document.querySelectorAll("textarea"));

  if (textareas.length === 0) {
    console.log("'Fix AO3 text editing' found no text areas, aborting.");
    return;
  }

  console.log("Checking text areas with 'Fix AO3 text editing' userscript...");
  for (const textarea of textareas) {
    cleanTextArea(textarea);
  }

  createWarning();
}

function cleanTextArea(textarea) {
  if (textarea.value === "") {
    return;
  }
  let newValue = textarea.value;
  // word-joiner
  newValue = newValue.replaceAll(/⁠/g, "&#8288;");
  // non-breaking space
  newValue = newValue.replaceAll(/ /g, "&nbsp;");
  // remove empty paragraphs
  newValue = newValue.replaceAll(/<p> *<\/p>/g, "\n\n");
  // remove ao3's special empty paragraphs
  newValue = newValue.replaceAll(/\n\n&nbsp;\n\n/g, "\n\n");
  // limit excessive new lines
  newValue = newValue.replaceAll(/\n( *\n)+/g, "\n\n");
  if (newValue !== textarea.value) {
    textarea.value = newValue;
  }
}

function findPostButtonSet() {
  const try1 = document.querySelector("fieldset.create");
  if (try1 !== null) {
    return try1;
  }
  const preview = document.querySelector('input[value="Preview"]');
  if (preview === null) {
    return null;
  }
  let parent = preview.parentElement;
  while (parent.tagName.toLowerCase() !== "fieldset") {
    parent = parent.parentElement;
  }
  return parent;
}

function createWarning() {
  const postButtonSet = findPostButtonSet();
  if (postButtonSet === null) {
    const preview = document.querySelector('input[value="Preview"]');
    // couldn't find a place to put the warning
    console.log("'Fix AO3 text editing' unable to display warning");
  }

  const warning = document.createElement("p");
  warning.innerHTML =
    '<strong>Note:</strong> If something unexpected is happening to your spacing when you edit, cancel the edit and try turning off the <a href="https://github.com/irrationalpie7/fandom-scripts/tree/main/tampermonkey" target="_blank">Fix AO3 text editing</a> userscript to see if that helps.';
  postButtonSet.insertBefore(warning, postButtonSet.firstChild);
}
