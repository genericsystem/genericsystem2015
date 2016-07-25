package org.genericsystem.reactor.gstag;

import java.util.function.Consumer;

import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class GSInputText extends GSTag {

	static final Logger log = LoggerFactory.getLogger(GSInputText.class);

	public GSInputText(GSTag parent) {
		super(parent, "input");
	}

	@Override
	protected InputTextHtmlDomNode createNode(String parentId) {
		return new InputTextHtmlDomNode(parentId);
	}

	public void bindAction(Consumer<GenericModel> applyOnModel) {
		addActionBinding(InputTextHtmlDomNode::getEnterProperty, applyOnModel);
	}
}