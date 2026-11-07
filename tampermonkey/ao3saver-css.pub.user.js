// ==UserScript==
// @name         ao3saver css injection
// @version      2026-11-07
// @description  Add css to ao3saver
// @author       irrationalpie7
// @match        https://ao3saver.com/works/*
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/ao3saver-css.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/ao3saver-css.pub.user.js
// ==/UserScript==

(async () => {
  "use strict";

  const css = `body {
  /* color tweaks */
  --primary: #eb9; /* #ddd */
  --background: #333;
  --links: #99f;
  --visited: #a8c;
  color: var(--primary);
  background-color: var(--background);

  /* viewport fix */
  --border-width: 3px;
  --font-size: 2.5em;
  font-size: var(--font-size);
}

a {
  color: var(--links);
}

a:visited {
  color: var(--visited);
}

#chapters, .userstuff {
  padding: .25em;
}

blockquote.userstuff {
  padding: 1em;
}

div,
span,
p {
  box-sizing: border-box;
}

.phone,
.breply,
.text {
  border: var(--border-width) solid var(--primary);
  display: block;
  padding: 0.5em;
  margin-block: 1em;
}

.phone .time,
.header {
  text-align: center;
  display: block;
}

.header {
  border-block-end: var(--border-width) solid var(--primary);
  width: 100%;
  margin-block-end: 1em;
}

.phone {
  /* width: 300px; */
  max-width: 100%;
}

.breply,
.text {
  width: max-content;
  max-width: 80%;
}

.breply {
  margin-left: auto;
}

.messagebody > br {
  display: none;
}

table {
  table-layout: fixed;
  width: 100%;
  max-width: 100%;
}

img {
  max-width: 100%;
}`;

  const style = document.createElement("style");
  style.innerHTML = css;
  document.head.appendChild(style);
})();
