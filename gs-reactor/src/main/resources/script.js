var wsocket;

function connect() {
	console.log("connect");
	wsocket = new WebSocket(serviceLocation);
	wsocket.binaryType = "arraybuffer";
	wsocket.onmessage = onMessageReceived;
	wsocket.onclose = onclose;
}

function onMessageReceived(evt) {
	console.log("JSON : " + evt.data);
	var message = JSON.parse(evt.data);
	var elt = document.getElementById(message.nodeId);
	switch (message.msgType) {
	case 'A':
		var parent = document.getElementById(message.parentId);
		if (parent == null) {
			console.error("Unreached parent on add. parent id  : "
					+ message.parentId + " for element : " + message.nodeId);
			break;
		}
		onCaseA(message, elt, parent);
		break;
	case 'R':
		elt.parentNode.removeChild(elt);
		break;
	case 'UT':
		if (elt.tagName == "INPUT") {
			if (message.type == "checkbox")
				elt.checked = message.checked;
			else
				elt.value = message.textContent;
		} else
			elt.innerHTML = message.textContent;
		break;
	case 'US':
		elt.selectedIndex = message.selectedIndex;
		break;
	case 'AC':
		elt.classList.add(message.styleClass);
		break;
	case 'RC':
		elt.classList.remove(message.styleClass);
		break;
	case 'AS':
		elt.style[message.styleProperty] = message.styleValue;
		break;
	case 'RS':
		elt.style.removeProperty(message.styleProperty);
		break;
	case 'AA':
		onCaseAA(message, elt);
		break;
	case 'RA':
		elt.removeAttribute(message.attributeName);
		if (message.attributeName == "value")
			elt.value = "";
		if (message.attributeName == "checked")
			elt.checked = false;
		break;
	default:
		alert("Unknown message received");
	}
}

function onCaseA(message, elt, parent) {
	elt = document.createElement(message.tagHtml);
	elt.id = message.nodeId;
	switch (message.tagHtml) {
	case "a":
		elt.href = "#";
		elt.onclick = function() {
			wsocket.send(JSON.stringify({
				msgType : "A",
				nodeId : this.id
			}));
			return false;
		};
		break;
	case "button":
		elt.onclick = function() {
			wsocket.send(JSON.stringify({
				msgType : "A",
				nodeId : this.id
			}));
		};
		break;

	case "select":
		elt.onchange = function() {
			wsocket.send(JSON.stringify({
				msgType : "U",
				nodeId : this.id,
				selectedIndex : this.selectedIndex
			}));
		}

		break;
	case "section":
		elt.classList.add("adding");
		break;
	case "div":
		elt.classList.add("adding");
		break;
	case "header":
		elt.classList.add("adding");
		break;
	case "footer":
		elt.classList.add("adding");
		break;
	};
	parent.insertBefore(elt, parent.children[message.nextId]);
}

function onCaseAA(message, elt) {
	switch (message.attributeName) {
	case "value":
		if (elt.value !== message.attributeValue)
			elt.value = message.attributeValue;
		break;
	case "checked":
		elt.checked = true;
		break;
	case "disabled":
		elt.disabled = true;
		break;
	case "list":
		onCaseAAList(message, elt);
		break;
	case "type":
		onCaseAAType(message, elt);
		break;
	default:
		elt.setAttribute(message.attributeName, message.attributeValue);
		break;
	}
}

function onCaseAAList(message, elt) {
	elt.setAttribute(message.attributeName, message.attributeValue);
	elt.oninput = function(e) {
		console.log("oninput");
		var val = elt.value;
		var opts = document.getElementById(elt.getAttribute("list")).childNodes;
		for (var i = 0; i < opts.length; i++) {
			if (opts[i].innerText === val) {
				wsocket.send(JSON.stringify({
					msgType : "U",
					nodeId : this.id,
					textContent : this.value
				}));
				wsocket.send(JSON.stringify({
					msgType : "A",
					nodeId : this.id
				}));
				break;
			}
		}
	}
}

function onCaseAAType(message, elt) {
	elt.setAttribute(message.attributeName, message.attributeValue);
	switch (message.attributeValue) {
	case "text":
	case "password":
		elt.onkeyup = function(e) {
			var code = (e.keyCode ? e.keyCode : e.which)
			if (code == 13) {
				wsocket.send(JSON.stringify({
					msgType : "A",
					nodeId : this.id
				}));
			} else {
				wsocket.send(JSON.stringify({
					msgType : "U",
					nodeId : this.id,
					textContent : this.value
				}));
			}
		};
		elt.onblur = function(e) {
			wsocket.send(JSON.stringify({
				msgType : "A",
				nodeId : this.id
			}));
		}
		break;
	case "checkbox":
	case "radio":
		elt.onchange = function() {
			wsocket.send(JSON.stringify({
				msgType : "U",
				nodeId : this.id,
				eltType : elt.type,
				checked : this.checked
			}));
		};
		elt.checked = message.checked;
		break;
	}
}

function onclose(evt) {
	alert("Socket has closed with code : " + evt.code);
}

function selectIndex(name) {
	var buttons = document.getElementsByName(name);
	for (var i = 0; i < buttons.length; i++) {
		if (buttons[i].checked) {
			return i;
		}
	}
}

window.onclick = function(event) {
	var modal = document.getElementsByClassName("modal");
	var i;
	for (i = 0; i < modal.length; i++) {
		var id = modal[i].id;
		if (modal[i].style.display == "flex" && event.target.id == id) {
			document.getElementsByName("close")[i].click();
		}
	}
}
