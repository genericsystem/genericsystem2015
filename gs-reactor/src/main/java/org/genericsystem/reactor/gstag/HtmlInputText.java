package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode.InputTextHtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.modelproperties.ActionDefaults;

public class HtmlInputText extends GSTag implements ActionDefaults<GenericModel> {

	public HtmlInputText(GSTag parent) {
		super(parent, "input");
	}

	@Override
	protected InputTextHtmlDomNode createNode(String parentId) {
		return new InputTextHtmlDomNode(parentId);
	}
}