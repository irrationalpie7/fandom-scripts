// ==UserScript==
// @name         Try fix workskins
// @namespace    irrationalpie
// @match        https://archiveofourown.org/*
// @grant        none
// @version      2026-09-12
// @author       irrationalpie
// @description  Automatically make some changes to workskin css on ao3 to try to make it work with a wider range of devices and site skins
// @updateURL   https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/try-fix-workskins.pub.user.js
// @downloadURL https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/try-fix-workskins.pub.user.js
// ==/UserScript==

(function () {
  "use strict";

  /** Helper for pasting into console */
  function findRule(selector) {
    const workskinCss = Array.from(document.styleSheets).filter(
      ({ cssRules }) =>
        Array.from(cssRules).every((r) => r.cssText.startsWith("#workskin ")),
    )[0];
    const workskinRules = Array.from(workskinCss.cssRules);
    return workskinRules.filter((r) => r.cssText.includes(selector));
  }

  /** Used to check whether it makes sense to have a max height or not */
  const containsMap = new Map();
  function containsText(selector) {
    const cached = containsMap.get(selector);
    if (cached === true || cached === false) {
      return cached;
    }
    const computed = !!document.querySelector(selector)?.textContent?.trim();
    containsMap.set(selector, computed);
    return computed;
  }

  const workskinCss = Array.from(document.styleSheets).filter(({ cssRules }) =>
    Array.from(cssRules).every((r) => r.cssText.startsWith("#workskin ")),
  )[0];

  if (!workskinCss) {
    console.log("hmmmm no workskin to fix");
    return;
  }
  console.log("transforming workskin!");

  function tryFixWorkskin(workskinCss) {
    const workskinRules = Array.from(workskinCss.cssRules);

    const deleteRules = [];

    workskinRules.forEach((r, i) => {
      // Leave screen-reader rules alone
      if (r.cssText.includes("clip: rect(0px, 0px, 0px, 0px);")) {
        return;
      }

      // Remove empty ::before / ::after rules
      if (
        r.cssText.includes('content: ""') &&
        (r.selectorText.includes("::before") ||
          r.selectorText.includes("::after"))
      ) {
        deleteRules.push(i);
        return;
      }
      process(r.style, containsText(r.selectorText));
    });

    deleteRules.reverse().forEach((i) => workskinCss.deleteRule(i));

    /** Fix wonky width assignments */
    function processWidth(style, containsText) {
      // If the person set a min width without setting a width,
      // they probably meant it as a width
      if (style.minWidth && !style.width) {
        style.width = style.minWidth;
        style.minWidth = "";
      }

      // If the person set a non-% max-width without setting a width
      // they probably meant it as a width
      if (!style.width && style.maxWidth && !style.maxWidth.endsWith("%")) {
        style.width = style.maxWidth;
        style.maxWidth = "";
      }

      // If the person set a non-% max-width with a % width, they probably
      // meant the values to be swapped
      if (
        /[0-9]/.test(style.maxWidth) &&
        !style.maxWidth.endsWith("%") &&
        style.width.endsWith("%")
      ) {
        const w = style.width;
        style.width = style.maxWidth;
        style.maxWidth = w;
      }

      // If they set a width, and the element can contain text,
      // set a min width to try to avoid the case where e.g. each
      // letter wraps to a new line
      style.minWidth = containsText && style.width ? "6rem" : "";

      // If there's a width, constrain with a max width
      if (style.width && !style.maxWidth.endsWith("%")) {
        style.maxWidth = "100%";
      }
    }

    function process(style, containsText) {
      // No major color
      if (style.color) {
        style.border = `2px solid ${style.color}`;
        style.color = "";
      }
      // (if both background color and color are specified, use bg for border)
      if (style.backgroundColor) {
        style.border = `2px solid ${style.backgroundColor}`;
        style.backgroundColor = "";
      }
      if (style.backgroundImage && containsText) {
        const matches = /rgb[^)]*\)/.exec(style.backgroundImage);
        if (matches && matches.length > 0) {
          style.border = `2px solid ${matches[0]}`;
        }
        style.backgroundImage = "";
      }
      style.textShadow = "";

      // Set reasonable widths
      processWidth(style, containsText);
      style.boxSizing = "border-box";
      if (style.display === "table") {
        // (table doesn't respect width)
        style.display = "flow-root";
      }
      if (style.whiteSpace === "nowrap") {
        style.whiteSpace = "";
      }

      // No scrolling
      style.overflow = "";
      if (style.height && containsText) {
        style.height = "fit-content";
      }
      if (style.maxHeight && containsText) {
        style.maxHeight = "fit-content";
      }

      // Reset some custom positioning weirdness
      if (style.position === "absolute") {
        style.position = "";
      }
      normalizeMargin(style, "Bottom");
      normalizeMargin(style, "Top");
      normalizeMargin(style, "Left");
      normalizeMargin(style, "Right");
      style.transform = "";
      // get rid of floats
      if (style.float === "left") {
        style.marginRight = "auto";
      }
      if (style.float === "right") {
        style.marginLeft = "auto";
      }
      if (style.float === "inline-start") {
        style.marginInlineEnd = "auto";
      }
      if (style.float === "inline-end") {
        style.marginInlineStart = "auto";
      }
      if (style.float && !style.display) {
        style.display = "block";
      }
      style.float = "";

      // Allow selecting text
      if (style.userSelect) {
        style.userSelect = "auto";
      }

      // No justify!
      if (style.textAlign === "justify") {
        style.textAlign = "left";
      }
    }

    function normalizeMargin(style, propertyName) {
      const padding = /[0-9]+/.exec(style[`padding${propertyName}`])?.[0] ?? 0;
      if (padding > 15) {
        style[`padding${propertyName}`] = "15px";
        style[`margin${propertyName}`] = "auto";
      }
      const margin = /[0-9]+/.exec(style[`margin${propertyName}`])?.[0] ?? 0;
      if (margin > 50) {
        style[`margin${propertyName}`] = "auto";
      }
      if (
        style[`margin${propertyName}`] &&
        style[`margin${propertyName}`].startsWith("-")
      ) {
        style[`margin${propertyName}`] = "";
      }
    }
  }

  tryFixWorkskin(workskinCss);
  // If the workskin has some rules that mask other rules, they may get skipped the first time
  tryFixWorkskin(workskinCss);
  tryFixWorkskin(workskinCss);

  // Set max width of 100% for 'container'-type elements
  workskinCss.insertRule(
    `#workskin [role="article"] .userstuff > div:not(#bogus#bogus#bogus),
     #workskin [role="article"].userstuff > div:not(#bogus#bogus#bogus),
     #workskin div#chapters > div.chapter > div.userstuff.module > div {
       max-width: 100%;
     }`,
  );

  // Remove empty paragraphs
  Array.from(document.querySelectorAll("p:not([class])"))
    .filter((p) => p.textContent.trim() === "")
    .filter((p) => p.childElementCount === 0)
    .forEach((p) => p.remove());
})();
