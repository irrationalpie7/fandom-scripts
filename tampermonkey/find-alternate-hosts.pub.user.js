// ==UserScript==
// @name         AO3 find alternate hosts
// @version      2026-10-31
// @description  Offer alternate fic-finding links
// @author       irrationalpie7
// @match        https://archiveofourown.org/*
// @updateURL    https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/find-alternate-hosts.pub.user.js
// @downloadURL  https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/find-alternate-hosts.pub.user.js
// ==/UserScript==

(async () => {
  "use strict";

  // Check for the maintenance warning
  const warning = document.querySelector("main h2");
  if (!warning || !warning.textContent.includes("archive is down")) {
    return;
  }

  const main = document.querySelector("main");
  const list = document.createElement("ul");
  const p = document.createElement("p");
  p.innerHTML = `Here are some alternate links which might help you access this page:`;
  main.appendChild(p);
  main.appendChild(list);

  /**
   *
   * @param {string} href
   * @returns {string}
   */
  function waybackify(href) {
    if (href.includes("web.archive.org")) {
      return href;
    }
    return `https://web.archive.org/web/20261028000000/${href}`;
  }

  /**
   * Add link
   * @param {string} href The page to link to
   * @param {string} text The link text (allows arbitrary html)
   * @param {HTMLElement} list Link list
   */
  function addLink(href, text, list) {
    const li = document.createElement("li");
    li.innerHTML = `<a href="${href}" target="_blank" rel="noopener noreferrer">${text}</a>`;
    list.appendChild(li);
  }

  /**
   *
   * @param {string} heading
   * @param {HTMLElement} list
   * @returns {HTMLElement} list
   */
  function addList(heading, list) {
    const li = document.createElement("li");
    li.textContent = heading;
    list.append(li);
    const newList = document.createElement("ul");
    li.append(newList);
    return newList;
  }

  /**
   *
   * @param {URL} url
   * @param {HTMLElement} list the base list
   * @returns {string | undefined} workId
   */
  function fillWaybackList(url, list) {
    const base = url.origin + url.pathname;
    const { workId } = /[/]works[/](?<workId>[0-9]+)/.exec(base)?.groups ?? {};

    if (!workId) {
      addLink(waybackify(url.href), `wayback this url (${url.href})`, list);
      if (base === url.href) {
        return;
      }
      addLink(waybackify(base), `wayback the base url (${base})`, list);
      return;
    }

    const waybackList = addList("Wayback Machine", list);
    const workUrl = new URL(`https://archiveofourown.org/works/${workId}`);

    addLink(
      waybackify(workUrl.href),
      `the work url ${workUrl.href}`,
      waybackList,
    );
    addLink(
      waybackify(`${workUrl.href}?view_full_work=true`),
      `the full work (?view_full_work=true)`,
      waybackList,
    );
    addLink(
      `https://web.archive.org/web/*/https://archiveofourown.org/works/${workId}`,
      `see versions from other dates`,
      waybackList,
    );
    addLink(
      `https://web.archive.org/web/*/archiveofourown.org/works/${workId}*`,
      `see other url variants`,
      waybackList,
    );
    addLink(
      `https://web.archive.org/web/*/archiveofourown.org/collections/*/works/${workId}*`,
      `see url variants in collections`,
      waybackList,
    );
    return workId;
  }

  const url = new URL(window.location.href);
  const workId = fillWaybackList(url, list);

  if (!workId) {
    p.innerHTML = `This doesn't appear to be a work page link, but you may be able to access it regardless:`;
    return;
  }

  const otherList = addList("Other sites", list);
  addLink(`https://ao3saver.com/works/${workId}`, "ao3saver link", otherList);
  addLink(
    `https://fichub.net/?q=https%3A%2F%2Farchiveofourown.org%2Fworks%2F${workId}`,
    "fichub.net link",
    otherList,
  );
  addLink(
    `https://archive.ph/https://archiveofourown.org/works/${workId}*`,
    "archive.today link",
    otherList,
  );
})();
