
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
		
		switch (message.msgType) {
			case 'A':
				var parent = document.getElementById(message.parentId);
				if (parent == null) {
					console.log("Unreached parent element id on add : "+message.nodeId)
					parent = document.getElementById("root");
				}
				var elt = document.createElement(message.tagHtml);
				elt.id = message.nodeId;
				elt.textContent = message.textContent;
				
				switch (message.tagHtml) 
				{						
					case "a": 		
						elt.href="#";						
						
					case "button":						
						buttonProcess(elt);	
						break;
						
					case "input": 
						inputProcess(elt,message);
						break;	
						
					case "option": 
						optionProcess(elt);
						break;
				}
	
				setEltStylesCLass(elt, message.styleClasses);
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
					else if (typeof message.removedStyleClass !== 'undefined') {
			
						elt.classList.remove(message.removedStyleClass);
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

	function onclose(evt) {
		alert("Socket close with code : " + evt.code);
	}
	
	function setEltStylesCLass(elt, styleClasses) {
		for (var i = 0; i < styleClasses.length; i++)
			elt.classList.add(styleClasses[i]);
	}
	
	function setEltStyles(elt, styles) {
		for (var attr in styles) {
			if(styles[attr]=="")
			{
			 	if (elt.style.removeProperty) 
			 	{
			 		elt.style.removeProperty (attr);
	            } 
	            else 
	            {
	            	elt.style.removeAttribute (attr);
	            }					
			}
			else
			{
				elt.style[attr]=styles[attr];
			}		    
		}
	}
	
	function buttonProcess(elt)
	{
		elt.onclick = function () {
			wsocket.send(JSON.stringify({
				msgType : "A",
				nodeId : elt.id
			}));
		};
	}
	
	function inputProcess(elt,message)
	{
		elt.type = message.type;
		switch (message.type) 
		{
			case "text": 
				elt.value = message.textContent;
				elt.onkeyup = function (e) {
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
		
			case "checkbox": 
				elt.onchange = function () {
					wsocket.send(JSON.stringify({
						msgType : "U",
						nodeId : elt.id,
						eltType : "checkbox",
						checked : elt.checked
					}));
				}
				elt.checked = message.checked;
				break;
		}
	}
	
	function optionProcess(elt)
	{
		elt.onclick = function () {
			wsocket.send(JSON.stringify({
				msgType : "A",
				nodeId : elt.id
			}));
		};
	}


	
