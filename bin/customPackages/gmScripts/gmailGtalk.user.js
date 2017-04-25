// ==UserScript==
// @name        Hide GTalk notification
// @namespace   gmailgtalk
// @include     https://mail.google.com/mail*
// @require     http://ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js
// @require  https://gist.github.com/raw/2625891/waitForKeyElements.js
// @version     1
// @grant       GM_addStyle
// ==/UserScript==
waitForKeyElements ('.bq0', actionFunction);
function actionFunction (jNode) {
  jNode.hide()
}
