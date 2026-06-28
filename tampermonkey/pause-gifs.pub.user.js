// ==UserScript==
// @name         AO3 pause gifs
// @version      1.0
// @description  Pause gifs with accessible "play" button
// @author       irrationalpie7
// @match        https://archiveofourown.org/*
// clang-format off
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/pause-gifs.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/pause-gifs.pub.user.js
// @require      https://cdn.jsdelivr.net/gh/adamchaboryk/gifa11y@2.2.2/dist/js/gifa11y.umd.min.js
// clang-format on
// ==/UserScript==

(async () => {
  "use strict";

  const gifa11y = new Gifa11y({
    container: "main",
    buttonBackground: "#000000",
    buttonBackgroundHover: "#404040",
    buttonIconColor: "white",
  });

  const images = Array.from(document.querySelectorAll("img"));
  images.forEach((image) => image.classList.add("gifa11y-paused"));
})();
