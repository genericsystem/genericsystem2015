package org.genericsystem.reactor.gstag;

import java.util.function.Consumer;

import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSButton extends GSTag {

	public GSButton(GSTag parent) {
		super(parent, "button");
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}

	public void bindAction(Consumer<GenericModel> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}
}
