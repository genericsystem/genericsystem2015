package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlDomNode;
import org.genericsystem.distributed.ui.HtmlDomNode.InputHtmlDomNode;

public class HtmlInputText extends HtmlElement<HtmlInputText, HtmlDomNode> {

	public HtmlInputText(HtmlElement<?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected InputHtmlDomNode createNode(Object parent) {
		return new InputHtmlDomNode(getWebSocket());
	}

	public <M> HtmlInputText bindAction(Consumer<M> applyOnModel) {
		addActionBinding(HtmlDomNode::getActionProperty, applyOnModel);
		return this;
	}

}
