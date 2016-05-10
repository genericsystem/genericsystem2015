package org.genericsystem.reactor.components;

import java.util.function.Consumer;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.HtmlElement.ActionHtmlNode;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlOption<M extends Model> extends HtmlElement<M, HtmlOption<M>, ActionHtmlNode> {

	public HtmlOption(HtmlElement<?, ?, ?> parent) {
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
