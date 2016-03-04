package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeInput;

public class HtmlInputText extends HtmlElement<HtmlInputText, HtmlNode> {

	public HtmlInputText(HtmlElement<?, ?> parent) {
		super(parent, HtmlNode.class);
	}

	@Override
	protected HtmlNodeInput createNode(Object parent) {
		return new HtmlNodeInput(getWebSocket());
	}

	public <M> HtmlInputText setAction(Consumer<M> applyOnModel) {
		addActionBinding(HtmlNode::getActionProperty, applyOnModel);
		return this;
	}

}
