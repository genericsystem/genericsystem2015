<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
	pageEncoding="ISO-8859-1"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<%@ taglib uri="http://www.springframework.org/tags/form" prefix="f"%>
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<link href="${pageContext.request.contextPath}/resources/css/style.css"
	rel="stylesheet" >
</head>
<body>
	<table class="maTable">
		<thead>
			<tr>
				<th scope="col"></th>
				<th scope="col">Car</th>
				<th scope="col">Power</th>
				<th scope="col">Color</th>
			</tr>
		</thead>
		<tfoot>
			<tr>
				<td colspan="4">
					<div id="">
						<f:form modelAttribute="sCar" method="POST"
							action="/gs-example-spring/">
							<f:input size="10" path="carName" />
							<f:input size="10" path="carPower" />
							<f:select path="carColor">
								<f:options items="${sColorBean.getColors()}" />
							</f:select>
							<input value="Add" class="bouton" name="action" type="submit">
						</f:form>
					</div>
				</td>
			</tr>
		</tfoot>
		<tbody>
			<c:forEach items="${sCars}" var="car">
				<tr id="${car.getTs()}">
					<td><input value="x" class="xBouton"
						onclick="process('remove',this);" type="submit"></td>
					<td><span class="customHeader">${car.getValue()}</span></td>
					<td><input value="${sCarBeanManager.getPower(car).getValue()}"
						size="10" onblur="process('power',this)" type="text"></td>
					<td><select name="" class="selectedItem" size="1"
						onchange="process('color',this)">
							<c:forEach items="${sColorBean.getColors()}" var="colorValue">
								<c:choose>
									<c:when
										test="${colorValue.getValue().equals(sCarBeanManager.getColor(car).getValue())}">
										<option value="${colorValue.getValue()}" selected>${colorValue.getKey()}</option>
									</c:when>
									<c:otherwise>
										<option value="${colorValue.getValue()}">${colorValue.getKey()}</option>
									</c:otherwise>
								</c:choose>
							</c:forEach>
					</select></td>
				</tr>
			</c:forEach>
		</tbody>
	</table>

	<input value="Save" class="bouton" name="action"
		onClick="process('save')" type="submit">
	<input value="Cancel" class="bouton" name="action"
		onClick="process('cancel')" type="submit">

	<script type="text/javascript">
	var xhr = getXMLHttpRequest();
	
	function getXMLHttpRequest() {	
	    var xhr = null;
	    if (window.XMLHttpRequest || window.ActiveXObject) {
	        if (window.ActiveXObject) {
	            try {
	                xhr = new ActiveXObject("Msxml2.XMLHTTP");
	            } catch(e) {
	                xhr = new ActiveXObject("Microsoft.XMLHTTP");
	            }
	
	        } else {
	            xhr = new XMLHttpRequest(); 
	        }
	    } 
	    return xhr;
	}
	
	function process(action,elem)
	{
		var params = "action="+action;
		var reload=false;
		
		switch(action) {
		    case "power":
		    case "color":
		    	params += "&id="+elem.parentNode.parentNode.id+"&value="+elem.value;
		        break;
		    case "remove":
		    	params += "&id="+elem.parentNode.parentNode.id;
		    	reload=true;
		        break;
		    case "save":
		    	reload=false;
		    	break;
		    case "cancel":
		    	reload=true;
		    	break;
		} 
		
		xhr.open("GET", "/gs-example-spring/update?"+params, true);		
		xhr.onreadystatechange = (function(xhr_local, reload_local) {
		    return function() {
		    	if (xhr_local.readyState != 4 && (xhr_local.status != 200 && xhr_local.status != 0) && xhr_local.responseText!="#" ) {

		        	alert("ERROR");

		        }else {
		        	if(reload_local)
					{
		        		window.location = window.location.href; 
					}		   		        	
		        	
		        }
		    }
		})(xhr,reload);

		xhr.send(null);	
		
	}
	
</script>
</body>
</html>