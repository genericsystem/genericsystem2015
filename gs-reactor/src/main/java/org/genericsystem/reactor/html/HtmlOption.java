package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.ActionHtmlNode;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlOption<M extends Model> extends HtmlElement<M, ActionHtmlNode> {

	public HtmlOption(HtmlElement<?, ?> parent) {
		super(parent, ActionHtmlNode.class);
	}

	@Override
	protected ActionHtmlNode createNode(Object parent) {
		return new ActionHtmlNode("option");
	}

	public HtmlOption<M> bindAction(Consumer<M> applyOnModel) {
		addActionBinding(ActionHtmlNode::getActionProperty, applyOnModel);
		return this;
	}
}
