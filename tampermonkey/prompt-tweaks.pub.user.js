// ==UserScript==
// @name         AO3 prompt display tweaks
// @version      2026-10-16
// @description  Fold claims and add prompt links
// @author       irrationalpie7
// @match        https://archiveofourown.org/collections/*
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/prompt-tweaks.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/prompt-tweaks.pub.user.js
// ==/UserScript==

(async () => {
  "use strict";

  // Fold completed claims
  const completedClaims = Array.from(
    document.querySelectorAll(
      ".prompt.index.group .work.listbox.group > ul > li",
    ),
  );
  completedClaims.forEach((blurb) => {
    const details = document.createElement("details");
    const summary = document.createElement("summary");
    summary.style.padding = "5px";
    summary.style.fontSize = "1rem";
    summary.textContent = blurb
      .querySelector("h4.heading")
      .textContent.trim()
      .replaceAll(/\s+/g, " ");
    details.appendChild(summary);
    while (blurb.firstChild) {
      details.appendChild(blurb.firstChild);
    }
    blurb.append(details);
  });

  // Add link buttons
  const blurbActions = Array.from(
    document.querySelectorAll(".prompt.index.group .blurb ul.actions"),
  );
  blurbActions.forEach((ul) => {
    const action = ul.querySelector("form").action;
    if (!action) {
      return;
    }
    const actionUrl = new URL(action);
    const id = actionUrl.searchParams.get("prompt_id");
    const [collection] =
      new RegExp("/collections/[^/]+/").exec(actionUrl.pathname) || [];
    if (!id || !collection) {
      return;
    }
    const li = document.createElement("li");
    li.innerHTML = `<a href="${collection}prompts/${id}">Link to prompt</a>`;
    ul.append(li);
  });
})();
