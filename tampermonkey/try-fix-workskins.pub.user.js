// ==UserScript==
// @name         Try fix workskins
// @namespace    irrationalpie
// @match        https://archiveofourown.org/*
// @grant        none
// @version      2026-09-08
// @author       irrationalpie
// @description  Automatically make some changes to workskin css on ao3 to try to make it work with a wider range of devices and site skins
// @updateURL   https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/try-fix-workskins.pub.user.js
// @downloadURL https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/try-fix-workskins.pub.user.js
// ==/UserScript==

(function () {
  "use strict";

  function tryFixWorkskin() {
    const workskinCss = Array.from(document.styleSheets).filter(
      ({ cssRules }) =>
        Array.from(cssRules).every((r) => r.cssText.startsWith("#workskin ")),
    )[0];

    if (!workskinCss) {
      console.log("hmmmm no workskin to fix");
      return;
    }
    console.log("transforming workskin!");

    const workskinRules = Array.from(workskinCss.cssRules);

    workskinRules.forEach((r) => {
      // console.log("before:");
      // console.log(r.cssText);
      process(r.style);
      // console.log("after:");
      // console.log(r.cssText);
      return;
    });

    console.log(workskinRules);

    function process(style) {
      // No major color
      if (style.color) {
        style.border = `1px solid ${style.color}`;
        style.color = "";
      }
      // (if both background color and color are specified, use bg for border)
      if (style.backgroundColor) {
        style.border = `1px solid ${style.backgroundColor}`;
        style.backgroundColor = "";
      }

      // Set reasonable widths
      style.boxSizing = "border-box";
      style.width = style.width || style.minWidth;
      if (!style.maxWidth.endsWith("%")) {
        style.maxWidth = "100%";
      }
      style.minWidth = "";
      if (style.whiteSpace === "nowrap") {
        style.whiteSpace = "";
      }
      if (style.display === "table") {
        // (table doesn't respect width)
        style.display = "flow-root";
      }

      // No scrolling
      style.overflow = "";
      if (style.height) {
        style.height = "fit-content";
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
      if (
        style[`margin${propertyName}`] &&
        style[`margin${propertyName}`].startsWith("-")
      ) {
        style[`margin${propertyName}`] = "";
      }
      // style[`padding${propertyName}`] = style[`margin${propertyName}`];
      // style[`margin${propertyName}`] = "";
    }
  }

  tryFixWorkskin();
  // If the workskin has some rules that mask other rules, they may get skipped the first time
  tryFixWorkskin();
  tryFixWorkskin();
  tryFixWorkskin();
  tryFixWorkskin();
  tryFixWorkskin();
})();
