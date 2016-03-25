package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlDomNode.ActionHtmlNode;
import org.genericsystem.distributed.ui.HtmlElement;

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
		return new ActionHtmlNode(getWebSocket(), "button");
	}

	public <M> HtmlButton bindAction(Consumer<M> applyOnModel) {
		addActionBinding(ActionHtmlNode::getActionProperty, applyOnModel);
		return this;
	}
}
