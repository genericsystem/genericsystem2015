
	var wsocket;

	function onMessageReceived(evt) {
		console.log("JSON : "+evt.data);
		var message = JSON.parse(evt.data);
		
		switch (message.msgType) {
			case 'A':
				var parent = document.getElementById(message.parentId);
				if (parent == null) {
					console.log("Unreached parent element id on add : "+message.nodeId)
					parent = document.getElementById("root");
				}
				var elt = document.createElement(message.tagHtml);
				elt.setAttribute("id", message.nodeId);
				elt.textContent = message.textContent;
				
				switch (message.tagHtml) {
					case "button": {
						elt.onclick = function click() {
							wsocket.send(JSON.stringify({
								msgType : "A",
								nodeId : elt.id
							}));
						};
						break;
					}
					case "a": {
						elt.onclick = function click() {
							wsocket.send(JSON.stringify({
								msgType : "A",
								nodeId : elt.id
							}));
						};
						elt.setAttribute("href", "#");
						break;
					}
					case "input": {
						elt.setAttribute("type", message.type);
						switch (message.type) {
						case "text": {
							elt.value = message.textContent;
							elt.onkeyup = function keyup(e) {
								var code = (e.keyCode ? e.keyCode : e.which)
								if (code == 13) {
									wsocket.send(JSON.stringify({
										msgType : "A",
										nodeId : elt.id
									}));
								} else {
									wsocket.send(JSON.stringify({
										msgType : "U",
										nodeId : elt.id,
										textContent : elt.value
									}));
								}
							}
							break;
						}
						case "checkbox": {
							elt.onchange = function change() {
								wsocket.send(JSON.stringify({
									msgType : "U",
									nodeId : elt.id,
									eltType : "checkbox",
									checked : elt.checked
								}));
								;
							}
							elt.checked = message.checked;
							break;
						}
						}
						break;
					}
					case "option": {
						elt.onclick = function () {
							wsocket.send(JSON.stringify({
								msgType : "A",
								nodeId : elt.id
							}));
						};
						break;
					}
					}
	
				setEltStylesCLass(elt, message.styleClass);
				setEltStyles(elt, message.style);
				parent.insertBefore(elt, document.getElementById(message.nextId));
				break;
			case 'R':
				var elt = document.getElementById(message.nodeId);
				if (elt != null) {
					elt.parentNode.removeChild(elt);
				}
				else {
					console.log("Unreached removed element id : "+message.nodeId)
				}
				break;
			case 'U':
				var elt = document.getElementById(message.nodeId);
				if (elt != null) {
					if (typeof message.textContent !== 'undefined') {
						if (elt.tagName == "INPUT"
								&& elt.getAttribute("type") == "text") {
							console.log("Receive : "+message.textContent);
							elt.value = message.textContent;
						}
						elt.textContent = message.textContent;
					}
					if (typeof message.styleClass !== 'undefined') {

						elt.classList.add(message.styleClass);
					}
					else if (typeof message.RemovedStyleClass !== 'undefined') {
			
							elt.classList.remove(message.RemovedStyleClass);
					}
					if (typeof message.style !== 'undefined') 
					{
						setEltStyles(elt, message.style);
					}
				} else {
					console.log("Unreached update element id : "+message.nodeId)
				}
				break;
			default:
				alert("Unknown message received");
			}
	}

	//-------------------------------------------------------------	
	function connect() {
		console.log("connecte");
		wsocket = new WebSocket(serviceLocation);
		wsocket.binaryType = "arraybuffer";
		wsocket.onmessage = onMessageReceived;
		wsocket.onclose = onclose;
	}

	function onclose(evt) {
		alert("Socket close with code : " + evt.code);
	}
	
	function setEltStylesCLass(elt, styleClass) {
		for (var i = 0; i < styleClass.length; i++)
			elt.classList.add(styleClass[i]);
	}
	
	function setEltStyles(elt, styles) {
		for (var attr in styles) {
			if(styles[attr]=="")
				{
				 	if (elt.style.removeProperty) {
				 		elt.style.removeProperty (attr);
		            } 
		            else {
		            	elt.style.removeAttribute (attr);
		            }
					
				}
			else
				{
					elt.style[attr]=styles[attr];
				}
		    
		}
	}


	
