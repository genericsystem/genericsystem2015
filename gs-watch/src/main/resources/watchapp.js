///**
// * 
// */
//
//// Overwriting the default function
//function onMessageReceived(evt) {
//	console.log("JSON : " + evt.data);
//	var message = JSON.parse(evt.data);
//	var elt = document.getElementById(message.nodeId);
//	switch (message.msgType) {
//	case 'A':
//		var parent = document.getElementById(message.parentId);
//		if (parent == null) {
//			console.log("Unreached parent on add. parent id  : "
//					+ message.parentId + " for element : " + message.nodeId);
//			break;
//		}
//		elt = document.createElement(message.tagHtml);
//		elt.id = message.nodeId;
//		switch (message.tagHtml) {
//		case "a":
//			elt.href = "#";
//			elt.onclick = function() {
//				wsocket.send(JSON.stringify({
//					msgType : "A",
//					nodeId : this.id
//				}));
//				return false;
//			};
//			break;
//		case "button":
//			elt.onclick = function() {
//				wsocket.send(JSON.stringify({
//					msgType : "A",
//					nodeId : this.id
//				}));
//			};
//			break;
//
//		case "select":
//			elt.onchange = function() {
//				wsocket.send(JSON.stringify({
//					msgType : "U",
//					nodeId : this.id,
//					selectedIndex : this.selectedIndex
//				}));
//			}
//
//			break;
//		case "section":
//			elt.classList.add("adding");
//			break;
//		case "div":
//			elt.classList.add("adding");
//			break;
//		case "header":
//			elt.classList.add("adding");
//			break;
//		case "footer":
//			elt.classList.add("adding");
//			break;
//		}
//		;
//		parent.insertBefore(elt, parent.children[message.nextId]);
//		break;
//	case 'R':
//		elt.parentNode.removeChild(elt);
//		break;
//	case 'UT':
//		if (elt.tagName == "INPUT") {
//			if (message.type == "checkbox")
//				elt.checked = message.checked;
//			else
//				elt.value = message.textContent;
//		} else
//			elt.innerHTML = message.textContent;
//		break;
//	case 'US':
//		elt.selectedIndex = message.selectedIndex;
//		break;
//	case 'AC':
//		elt.classList.add(message.styleClass);
//		break;
//	case 'RC':
//		elt.classList.remove(message.styleClass);
//		break;
//	case 'AS':
//		elt.style[message.styleProperty] = message.styleValue;
//		break;
//	case 'RS':
//		elt.style.removeProperty(message.styleProperty);
//		break;
//	case 'AA':
//		switch (message.attributeName) {
//		case "value":
//			if (elt.value !== message.attributeValue)
//				elt.value = message.attributeValue;
//			break;
//		case "checked":
//			elt.checked = true;
//			break;
//		case "disabled":
//			elt.disabled = true;
//			break;
//		case "list":
//			elt.setAttribute(message.attributeName, message.attributeValue);
//			elt.oninput = function(e) {
//				console.log("oninput");
//				var val = elt.value;
//				var opts = document.getElementById(elt.getAttribute("list")).childNodes;
//				for (var i = 0; i < opts.length; i++) {
//					if (opts[i].innerText === val) {
//						console.log(">>> case 1");
//						wsocket.send(JSON.stringify({
//							msgType : "U",
//							nodeId : this.id,
//							textContent : this.value
//						}));
//						console.log(">>> case 2");
//						wsocket.send(JSON.stringify({
//							msgType : "A",
//							nodeId : this.id
//						}));
//						break;
//					}
//				}
//			}
//			break;
//		case "type":
//			elt.setAttribute(message.attributeName, message.attributeValue);
//			switch (message.attributeValue) {
//			case "text":
//			case "password":
//				elt.onkeyup = function(e) {
//					console.log("onkeyup");
//					var code = (e.keyCode ? e.keyCode : e.which)
//					if (code == 13) {
//						console.log(">>> case 3");
//						wsocket.send(JSON.stringify({
//							msgType : "A",
//							nodeId : this.id
//						}));
//					} /*else {
//						console.log(">>> case 4");
//						wsocket.send(JSON.stringify({
//							msgType : "U",
//							nodeId : this.id,
//							textContent : this.value
//						}));*/
//					}
//				};
//				elt.onblur = function(e) {
//					console.log("onblur");
//					wsocket.send(JSON.stringify({
//						msgType : "A",
//						nodeId : this.id
//					}));
//				}
//				break;
//			case "checkbox":
//				elt.onchange = function() {
//					wsocket.send(JSON.stringify({
//						msgType : "U",
//						nodeId : this.id,
//						eltType : elt.type,
//						checked : this.checked
//					}));
//				};
//				elt.checked = message.checked;
//				break;
//
//			case "radio":
//				elt.name = document.getElementById(message.parentId).parentNode.id;
//				elt.onclick = function() {
//					wsocket.send(JSON.stringify({
//						msgType : "U",
//						nodeId : this.parentNode.parentNode.id,
//						eltType : elt.type,
//						selectedIndex : selectIndex(this.name)
//					}));
//				};
//				break;
//			}
//			break;
//		default:
//			elt.setAttribute(message.attributeName, message.attributeValue);
//			break;
//		}
//		break;
//	case 'RA':
//		elt.removeAttribute(message.attributeName);
//		if (message.attributeName == "value")
//			elt.value = "";
//		if (message.attributeName == "checked")
//			elt.checked = false;
//		break;
//	default:
//		alert("Unknown message received");
//	}
//}