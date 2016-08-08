package org.genericsystem.reactor.gstag;

import java.util.function.Consumer;

import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.gs.SelectionDefaults;
import org.genericsystem.reactor.model.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSHyperLink extends GSTag implements SelectionDefaults {

	public GSHyperLink(GSTag parent) {
		super(parent, "a");
	}

	public GSHyperLink(GSTag parent, String text) {
		super(parent, "a");
		setText(text);
	}

	public GSHyperLink(GSTag parent, String text, Consumer<GenericModel> action) {
		this(parent, text);
		bindAction(action);
	}

	public void bindAction(Consumer<GenericModel> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	}

	@Override
	protected ActionHtmlNode createNode(String parentId) {
		return new ActionHtmlNode(parentId);
	}

}
