package org.genericsystem.carcolor;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.ActionHtmlNode;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class GSModalButton extends GSTag{

	public GSModalButton(GSTag parent, GSSection gSection) {
		super(parent, "button");
		bindAction(model -> {
			gSection.getProperty(ReactorStatics.DISPLAY, model).setValue("flex");
		});
		
	}		

	public void bindAction(Consumer<GenericModel> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}
	
}
