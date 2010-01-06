var isNav4 = (navigator.appName == "Netscape" && parseInt(navigator.appVersion) == 4)
var isNav4Min = (navigator.appName == "Netscape" && parseInt(navigator.appVersion) >= 4)
var isIE4Min = (navigator.appName.indexOf("Microsoft") != -1 && parseInt(navigator.appVersion) >= 4)

if (isNav4) { document.captureEvents(Event.MOUSEUP) }

function getRangeObject(selectionObject) {
	if (selectionObject.getRangeAt){
	  	alert(100);
		return selectionObject.getRangeAt(0);
	} else { // Safari!
	        var range = document.createRange();
		range.setStart(selectionObject.anchorNode,selectionObject.anchorOffset);
		range.setEnd(selectionObject.focusNode,selectionObject.focusOffset);
	  	alert(selectionObject.anchorNode.id);
		return range;
	}
}


function getText(sel){
  if (sel) {
    return (lineOfNode(sel.anchorNode) + ":" + sel);
  } 
  return "";
}


function showSelection(document) {
    if (isNav4Min) {
      document.forms[0].selectedText.value = getText(document.getSelection())
    } else if (isIE4Min) {
        if (document.selection) {
	    document.forms[0].selectedText.value = "IE4min: \n " + document.selection.createRange().text
            event.cancelBubble = true
        }
    }
}

function lineOfNode(n){
  var nid;
  if (n) {
    if (n.id) { nid = n.id; } else { nid = "" };
    if (nid.substr(0,5) == "line:") {
      return nid.substr(5)
    } else {
      return (lineOfNode(n.parentNode))
    } 
  }
  return "dog";
}
<!--
<SCRIPT LANGUAGE="JavaScript">
var isNav4 = (navigator.appName == "Netscape" && parseInt(navigator.appVersion) == 4)
var isNav4Min = (navigator.appName == "Netscape" && parseInt(navigator.appVersion) >= 4)
var isIE4Min = (navigator.appName.indexOf("Microsoft") != -1 && parseInt(navigator.appVersion) >= 4)


function showSelection() {
    if (isNav4Min) {
        document.forms[0].selectedText.value = "NAV4min: " + document.getSelection()
    } else if (isIE4Min) {
        if (document.selection) {
	    document.forms[0].selectedText.value = "IE4min: " + document.selection.createRange().text
            event.cancelBubble = true
        }
    }
}
if (isNav4) {
    document.captureEvents(Event.MOUSEUP)
}
document.onmouseup = showSelection
</SCRIPT> -->


