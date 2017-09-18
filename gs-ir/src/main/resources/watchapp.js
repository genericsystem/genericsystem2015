/**
 * 
 */

// Overwriting the default function
function onclose(evt) {
	console.error("Socket has closed with code : " + evt.code);
}