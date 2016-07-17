
var wsocket;

function connect() {
	console.log("connecte");
	wsocket = new WebSocket(serviceLocation);
	wsocket.binaryType = "arraybuffer";
	wsocket.onmessage = onMessageReceived;
	wsocket.onclose = onclose;
}

function onMessageReceived(evt) {
	console.log("JSON : "+evt.data);
	var message = JSON.parse(evt.data);
	var elt = document.getElementById(message.nodeId);
	switch (message.msgType) {
	case 'A':
		var parent = document.getElementById(message.parentId);
		if (parent == null) {
			console.log("Unreached parent element id on add : "+message.nodeId)
			parent = document.getElementById("root");
		}
		elt = document.createElement(message.tagHtml);
		elt.id = message.nodeId;
		switch (message.tagHtml) {						
		case "a": 		
			elt.href="#";
			elt.onclick = function () {
				wsocket.send(JSON.stringify({
					msgType : "A",
					nodeId : this.id
				}));
			};
			break;
		case "button":
			elt.onclick = function () {
			wsocket.send(JSON.stringify({
				msgType : "A",
				nodeId : this.id
			}));
		};
		break;
		case "input": 
			elt.type = message.type;
			switch (message.type) 
			{
			case "text": 
				elt.onkeyup = function (e) {
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
			break;

			case "checkbox": 
				elt.onchange = function () {
				wsocket.send(JSON.stringify({
					msgType : "U",
					nodeId : this.id,
					eltType : elt.type,
					checked : this.checked
				}));
			};
			elt.checked = message.checked;
			break;

			case "radio":
				elt.name = document.getElementById(message.parentId).parentNode.id;
				elt.onclick = function () {
					wsocket.send(JSON.stringify({
						msgType : "U",
						nodeId : this.parentNode.parentNode.id,
						eltType : elt.type,
						selectedIndex : selectIndex(this.name)
					}));
				};
				break;
			}
			break;

		case "select": 
			elt.onchange = function () {
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
		case "header": 
			elt.classList.add("adding");
			break;
		case "footer": 
			elt.classList.add("adding");
			break;
		};
		parent.insertBefore(elt, parent.children[message.nextId]);
		break;
	case 'R':
		if (elt != null) {
			elt.classList.add("removing");
			//setTimeout(function(){ 
				elt.parentNode.removeChild(elt);
			//}, 2000);
		}
		else {
			console.log("Unreached removed element id : "+message.nodeId)
		}
		break;
	case 'UT':
		if (elt.tagName == "INPUT") {
			if (message.type == "checkbox")
				elt.checked = message.checked;
			else
				elt.value = message.textContent;
		}
		else
			elt.textContent = message.textContent;
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
		elt.style[message.styleProperty]=message.styleValue;
		break;
	case 'RS':
		elt.style.removeProperty(message.styleProperty);
		break;
	case 'AA':
		elt.setAttribute(message.attributeName, message.attributeValue);
		if (message.attributeName == "value")
			elt.value = message.attributeValue;
		if (message.attributeName == "checked")
			elt.checked = message.attributeValue;
		break;
	case 'RA':
		elt.removeAttribute(message.attributeName);
		if (message.attributeName == "value")
			elt.value = "";
		if (message.attributeName == "checked")
			elt.checked = false;
		break;
	default :
		alert("Unknown message received");
	}
}	

function onclose(evt) {
	alert("Socket has closed with code : " + evt.code);
}

function selectIndex(name){
	var buttons = document.getElementsByName(name);
	for (var i = 0; i < buttons.length; i++) {
		if(buttons[i].checked){
			return i;
		}
	}
}
