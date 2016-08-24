package org.genericsystem.reactor.gstag;

import java.util.function.Consumer;

import org.genericsystem.reactor.HtmlDomNode.ActionHtmlNode;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.SwitchDefaults;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHyperLink extends GSTag implements SelectionDefaults, SwitchDefaults {

	public HtmlHyperLink(GSTag parent) {
		super(parent, "a");
	}

	public HtmlHyperLink(GSTag parent, String text) {
		super(parent, "a");
		setText(text);
	}

	public HtmlHyperLink(GSTag parent, String text, Consumer<GenericModel> action) {
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
