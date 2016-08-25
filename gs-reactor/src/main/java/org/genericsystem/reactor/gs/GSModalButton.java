package org.genericsystem.reactor.gs;

import java.util.function.Consumer;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.ActionHtmlNode;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.model.GenericModel;

public class GSModalButton extends GSTag {

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
