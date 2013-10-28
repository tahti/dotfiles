/**
 * @version	1.2
 * @author	Allen Choong and Piotr Kordy
 * @created	2010-09-25
 *
 * Enhance GMail Basic HTML with "select all" and link to Calendar
 *
 * Changelog:
 * 2013-05-03	1.2		Add the include for multiple login
 * 2010-10-01	1.1		Using addEventListener() to solve the problem for Google Chrome, because
 * Chrome does not use the unsafeWindow as the source. Thus, now compatible to both Firefox and Chrome
 */

// ==UserScript==
// @name           GMail Basic HTML Enhancement
// @namespace      http://allencch.wordpress.com
// @description    Enhance GMail Basic HTML with "select all"
// @include        https://mail.google.com/mail/*/h/*
// @include        https://mail.google.com/mail/h/*
// @grant          none
// ==/UserScript==


gmSelect = function(check) {
	var inputs = document.getElementsByTagName('input');
	for(var i=0;i<inputs.length;i++) {
		if(inputs[i].type == 'checkbox') {
			inputs[i].checked = check;
		}
	}
}

gmSelectCheck = function() {
	select = this.innerHTML;
	if(select == "Select All")
		gmSelect(true);
	else
		gmSelect(false);
}


window.gmFunction = function() {
	var anchors,newElement;	
	anchors = document.getElementsByTagName('a');
	
	var refresh = new Array();
	

	newElement = document.createElement('div');
	newElement.setAttribute('name','gm_elem');
	var newLink = document.createElement('a');
  var linkText = document.createTextNode("Calendar");
  var spaceText = document.createTextNode("\xA0\xA0");
  newLink.appendChild(linkText);
  newLink.title = "Calendar";
  newLink.href = "http://www.google.com/calendar?hl=en-GB";
  newLink.target = "_blank";
  var nobr=document.getElementById("gbar").firstChild;
  nobr.insertBefore(spaceText.cloneNode(true),nobr.firstChild);  
  nobr.insertBefore(newLink.cloneNode(true),nobr.firstChild);

	
	
	//Find all the refresh
	for(var i=0,j=0;i<anchors.length;i++) {
		if(anchors[i].innerHTML.search(/Refresh/) >= 0) {
			anchors[i].parentNode.insertBefore(newElement.cloneNode(true),anchors[i].nextSibling);
		}
		//if(anchors[i].innerHTML.search(/Search/) >= 0) {
			//anchors[i].parentNode.insertBefore(newLink.cloneNode(true),anchors[i].nextSibling);
		//}
	}
	
	
	var gmElem = document.getElementsByName('gm_elem');
	for(var i=0;i<gmElem.length;i++) {
		gmElem[i].innerHTML = '<a name="gm_select" href="javascript:">Select All</a> ';
		gmElem[i].innerHTML += '<a name="gm_select" href="javascript:">Select None</a>';
	}
	
	//Using addEventListener() to solve the Chromium problem
	var select = document.getElementsByName('gm_select');
	for(var i=0;i<select.length;i++) {
		select[i].addEventListener('click',gmSelectCheck,false);
	}
}

window.setTimeout(gmFunction(),1*1000);
