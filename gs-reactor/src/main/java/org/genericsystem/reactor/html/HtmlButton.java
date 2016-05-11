package org.genericsystem.reactor.html;

import java.util.function.Consumer;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.HtmlElement.ActionHtmlNode;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton<M extends Model> extends HtmlElement<M, HtmlButton<M>, ActionHtmlNode> {

	public HtmlButton(HtmlElement<?, ?, ?> parent) {
		super(parent, ActionHtmlNode.class);
	}

	@Override
	protected ActionHtmlNode createNode(Object parent) {
		return new ActionHtmlNode("button");
	}

	public HtmlButton<M> bindAction(Consumer<M> applyOnModel) {
		addActionBinding(ActionHtmlNode::getActionProperty, applyOnModel);
		return this;
	}
}
