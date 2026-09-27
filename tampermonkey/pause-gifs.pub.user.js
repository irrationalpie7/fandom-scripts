// ==UserScript==
// @name         AO3 pause gifs
// @version      1.1
// @description  Pause gifs with accessible "play" button
// @author       irrationalpie7
// @match        https://archiveofourown.org/*
// clang-format off
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/pause-gifs.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/pause-gifs.pub.user.js
// clang-format on
// ==/UserScript==

(async () => {
  "use strict";

  const setupScript = document.createElement("script");
  setupScript.type = "module";
  setupScript.innerHTML = `import Gifa11y from "https://cdn.jsdelivr.net/gh/adamchaboryk/gifa11y@2.2.2/dist/js/gifa11y.esm.min.js";
  const gifa11y = new Gifa11y({
    container: 'main',
    missingAltWarning: false,
    initiallyPaused: true
  });
  window.gifa11y = gifa11y;
  setTimeout(() => {
    gifa11y.findNew();
  }, 3_000);`;
  document.head.appendChild(setupScript);
})();
