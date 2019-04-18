// ==UserScript==
// @name         New Userscript
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  try to take over the world!
// @author       You
// @match        https://www.google.com/search*
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    setTimeout(()=> {
        document.querySelectorAll('h1,h2,h3,h4,h5,h6').forEach(function(el){
            if(/Twitter Results/gi.test(el.innerHTML.toString()) && el.nextElementSibling){
                el.nextElementSibling.remove();
            }
        });
        document.querySelectorAll('h1,h2,h3,h4,h5,h6').forEach(function(el){
            if(/Instagram Results/gi.test(el.innerHTML.toString()) && el.nextElementSibling){
                el.nextElementSibling.remove();
            }
        });
        document.querySelectorAll('h1,h2,h3,h4,h5,h6').forEach(function(el){
            if(/People also ask/gi.test(el.innerHTML.toString()) && el.nextElementSibling){
                el.nextElementSibling.remove();
                el.remove();
            }
        });
    // Your code here...
    }, 50);
})();