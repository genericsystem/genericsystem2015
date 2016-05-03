package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.ActionHtmlNode;
import org.genericsystem.distributed.ui.Model;

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
