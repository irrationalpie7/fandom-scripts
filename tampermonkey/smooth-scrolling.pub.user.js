// ==UserScript==
// @name        Smooth scroll
// @namespace   irrationalpie scripts
// @match       https://archiveofourown.org/*
// @grant       none
// @version     1.0
// @author      irrationalpie
// @updateURL   https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/smooth-scrolling.pub.user.js
// @downloadURL https://github.com/irrationalpie7/fandom-scripts/raw/main/tampermonkey/smooth-scrolling.pub.user.js
// @description Scroll slowly down ao3 as you read
// ==/UserScript==

// 0: off (value is ignored)
// 1: 1px/80ms (slow)
// 2: 1px/30ms (faster)
const speeds = [0, 80, 30];
// means this script will trigger when you ctrl+x
const shortcut = "x";
// track current state
const scrolling = {key: -1, speedIndex: 0};

document.addEventListener(
  "keydown",
  (event) => {
    if (event.ctrlKey && event.key === shortcut) {
      // if the current speed isn't off, halt scrolling
      if (scrolling.speedIndex !== 0){
        clearInterval(scrolling.key);
      }
      // go to the next level of scrolling (faster, or if you're already fastest, off)
      scrolling.speedIndex = (scrolling.speedIndex + 1) % speeds.length;
      if (scrolling.speedIndex !== 0){
        scrolling.key = setInterval(() => window.scrollBy({
            // scroll down by 1 px
            top: 1,
            left: 0,
            behavior: "instant",
          }),
          // number of ms to wait between scrolls
          speeds[scrolling.speedIndex]);
        if (scrolling.speedIndex === 1){
          console.log("now scrolling slowly");
        } else {
          console.log("now scrolling faster");
        }
      } else {
        console.log("scrolling paused");
      }
    }
  },
  false
);
console.log("ready to scroll! ctrl+x to begin");
