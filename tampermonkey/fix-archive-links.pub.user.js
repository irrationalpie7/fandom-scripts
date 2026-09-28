// ==UserScript==
// @name         AO3 fix archive.org links
// @version      0.1
// @description  Detect and fix fragile archive.org links
// @author       irrationalpie7
// @match        https://archiveofourown.org/*
// clang-format off
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/fix-archive-links.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/fix-archive-links.pub.user.js
// clang-format on
// ==/UserScript==

(async () => {
  "use strict";

  // Parse current url to make sure we're on a work page.
  const url = window.location.href;
  const workPageRegex = new RegExp(
    "^https://archiveofourown[.]org(/.*)?/(works|chapters)/[0-9]+",
  );
  if (!workPageRegex.test(url)) {
    return;
  }

  // Find fragile links
  const fragileLinks = Array.from(document.querySelectorAll("[src],[href]"))
    .map((el) => ({
      el: el,
      tag: el.tagName,
      /** @type {string} */
      link: el.src || el.href,
      linkType: el.src ? "src" : "href",
    }))
    .filter((link) => {
      if (!link.link) {
        return false;
      }
      const url = new URL(link.link);
      link.link = url.href;
      const archiveOrgFragile =
        /https?:\/\/[^/]+archive\.org\/\d+\/items\/(?<details>[^/]+)\/(?<rest>.*)/;
      const match = archiveOrgFragile.exec(link.link).groups;
      if (!match) {
        return false;
      }
      const { details, rest } = match.groups;

      link.details = `https://archive.org/details/${details}`;
      link.newUrl = `https://archive.org/download/${details}/${rest}`;
      link.el.setAttribute(link.linkType, link.newUrl);
      return true;
    });

  // No fragile links
  if (fragileLinks.length === 0) {
    return;
  }

  const link = fragileLinks[0];
  const commentText = `It looks like you're using archive.org as a host and are using the <a href="https://archive.org/help/audio.php">fragile form of the archive.org link</a>, which stops working after a while. Instead of "${link.link}", your link(s) should look like "${link.newUrl}". You can get the right link format by going to <a href="${link.details}">${link.details}</a>, clicking "show all" under the download options, then right-clicking or long-pressing the file you want to link to and copying the link.`;

  // Comment box
  /** @type {HTMLTextAreaElement} */
  const commentBox = document.querySelector("#feedback #add_comment textarea");
  // Edit button (indicates that viewer is work author)
  const edit = document.querySelector(".work.navigation.actions li.edit");

  // Inform the viewer
  const dl = document.querySelector("dl.work.meta.group");
  const dt = document.createElement("dt");
  dt.textContent = "Fragile links detected";
  dl.appendChild(dt);
  const dd = document.createElement("dd");

  if (edit) {
    dd.innerHTML = `Hi! ${commentText}`;
  } else if (commentBox) {
    const button = document.createElement("button");
    dd.innerHTML = `<strong>Note:</strong> Hi! ${commentText}`;
    button.textContent = "Add note to comment box";
    button.onclick = () => {
      if (commentBox.value) {
        commentBox.value = `${commentBox.value}\n\n${commentText}`;
      } else {
        commentBox.value = `Hi! ${commentText}`;
      }
    };
    dd.insertBefore(button, dd.firstChild);
  } else {
    dd.innerHTML = `This author doesn't appear to allow commenting. If you have another way to contact them, you could pass on this message: Hi! ${commentText}`;
  }
  dl.appendChild(dd);
})();
