package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.ActionHtmlNode;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlButton extends HtmlElement<HtmlButton, ActionHtmlNode> {

	public HtmlButton(HtmlElement<?, ?> parent) {
		super(parent, ActionHtmlNode.class);
	}

	@Override
	protected ActionHtmlNode createNode(Object parent) {
		return new ActionHtmlNode("button");
	}

	public <M> HtmlButton bindAction(Consumer<M> applyOnModel) {
		addActionBinding(ActionHtmlNode::getActionProperty, applyOnModel);
		return this;
	}
}
