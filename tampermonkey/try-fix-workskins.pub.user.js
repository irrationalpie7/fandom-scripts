// ==UserScript==
// @name         Try fix workskins
// @namespace    irrationalpie
// @match        https://archiveofourown.org/*
// @grant        none
// @version      2026-10-16
// @author       irrationalpie
// @description  Automatically make some changes to workskin css on ao3 to try to make it work with a wider range of devices and site skins
// @updateURL   https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/try-fix-workskins.pub.user.js
// @downloadURL https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/try-fix-workskins.pub.user.js
// ==/UserScript==

(function () {
  "use strict";

  /**
   * Helper for pasting into console
   * @param {string} selector
   * @returns {CSSRule[]}
   */
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
  /**
   * Check whether an element has text content
   * @param {string} selector
   * @returns {boolean}
   */
  function containsText(selector) {
    const cached = containsMap.get(selector);
    if (cached === true || cached === false) {
      return cached;
    }
    const computed = !!document.querySelector(selector)?.textContent?.trim();
    containsMap.set(selector, computed);
    return computed;
  }

  const workskin = document.getElementById("workskin");
  if (!workskin) {
    console.log("not a work--nothing to fix");
    return;
  }

  // Remove empty paragraphs
  Array.from(document.querySelectorAll("p:not([class])"))
    .filter((p) => p.textContent.trim() === "")
    .filter((p) => p.childElementCount === 0)
    .forEach((p) => p.remove());

  // Remove gratuitous non-breaking spaces
  const walker = document.createTreeWalker(workskin, NodeFilter.SHOW_TEXT);
  while (walker.nextNode()) {
    // Allow &nbsp; if there's at least three in a row (e.g., ascii art)
    if (/   +/.test(walker.currentNode.textContent)) {
      continue;
    }
    walker.currentNode.textContent = walker.currentNode.textContent.replaceAll(
      / /g,
      " ",
    );
  }

  // Remove gratuitous anchor hrefs
  const weirdAnchors = Array.from(
    document.querySelectorAll("#workskin a[href]"),
  ).filter((a) => {
    const href = a.getAttribute("href");
    return !(
      href === null ||
      href.startsWith("http://") ||
      href.startsWith("https://") ||
      href.startsWith("#") ||
      href.startsWith("/")
    );
  });
  if (weirdAnchors.length > 0) {
    console.log(
      "Getting rid of faulty links:",
      new Set(weirdAnchors.map((a) => a.href)),
    );
  }
  weirdAnchors.forEach((a) => a.removeAttribute("href"));

  /**
   * @type {CSSStyleSheet}
   */
  const workskinCss = document.querySelector("#work-skin style")?.sheet; // Note that the parent element is #work-skin, a wrapper outside #workskin

  if (!workskinCss) {
    console.log("hmmmm no workskin to fix");
    return;
  }
  console.log("transforming workskin!");

  /**
   * Try fixing the workskin
   *
   * @param {CSSStyleSheet} workskinCss
   */
  function tryFixWorkskin(workskinCss) {
    const workskinRules = Array.from(workskinCss.cssRules);

    /**
     * @type {number[]}
     */
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
      /**
       * @type{CSSStyleProperties}
       */
      const style = r.style;
      // Experimental minimum spacing around pseudo elements
      if (
        r.selectorText.includes("::before") ||
        r.selectorText.includes("::after")
      ) {
        if (
          style.position === "absolute" ||
          style.left ||
          style.right ||
          style.top ||
          style.bottom
        ) {
          style.margin = ".5em";
        }
      }
      process(style, containsText(r.selectorText));
    });

    deleteRules.reverse().forEach((i) => workskinCss.deleteRule(i));

    /**
     * Fix wonky width assignments
     * @param {CSSStyleProperties} style
     * @param {string} containsText
     */
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

    /**
     * Update the style according to some heuristics
     *
     * @param {CSSStyleProperties} style the style properties to update
     * @param {boolean} containsText whether an element associated with this rule has any text
     */
    function process(style, containsText) {
      // No major color
      if (style.color) {
        style.border = `2px groove ${style.color}`;
        style.color = "";
      }
      // (if both background color and color are specified, use bg for border)
      if (style.backgroundColor) {
        style.border = `2px groove ${style.backgroundColor}`;
        style.backgroundColor = "";
      }
      if (style.backgroundImage) {
        if (containsText) {
          const matches = /rgb[^)]*\)/.exec(style.backgroundImage);
          if (matches && matches.length > 0) {
            style.border = `2px groove ${matches[0]}`;
          }
          style.backgroundImage = "";
        } else {
          // element may become "inline-ish" and not respect height;
          // this prevents the image from getting cropped, though if
          // it would be cropped it'll instead be tiny.
          style.backgroundSize = "contain";
        }
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
      ["Top", "Bottom", "Left", "Right"].forEach((pos) =>
        normalizeMargin(style, pos),
      );
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
      if (style.float && (!style.display || style.display.includes("inline"))) {
        const replacement = style.display
          .replace(/-inline-/, "-")
          .replace(/inline-?/, "")
          .trim();
        style.display = replacement || "block";
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

    /**
     *
     * @param {CSSStyleProperties} style
     * @param {'Bottom'|'Top'|'Left'|'Right'} propertyName
     */
    function normalizeMargin(style, propertyName) {
      const padding = /[0-9]+/.exec(style[`padding${propertyName}`])?.[0] ?? 0;
      // there's some trick with padding + percents used with background images
      if (padding > 15 && !style[`padding${propertyName}`].includes("%")) {
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
})();
