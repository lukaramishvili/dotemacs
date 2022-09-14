// ==UserScript==
// @name     Unnamed Script 249435
// @version  1
// @grant    none
// ==/UserScript==

if(document.location.href.indexOf('facebook') !== -1){

  setInterval(() => {
    let suggestedPosts = document.querySelectorAll('[data-pagelet*="FeedUnit_"]')
    for(let i = 0; i < suggestedPosts.length; i++){
      let el = suggestedPosts[i];
      if(el.innerHTML.indexOf('Suggested for you') !== -1){
        el.remove()
      }
    }
  }, 3000);
  
}
