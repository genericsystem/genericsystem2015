package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeInput;

public class HtmlInputText extends HtmlElement<HtmlInputText> {

	public HtmlInputText(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNodeInput createNode(Object parent) {
		HtmlNodeInput input = new HtmlNodeInput(getWebSocket(), "text");
		input.getTag().set("input");
		return input;
	}

	public <M> HtmlInputText setAction(Consumer<M> applyOnModel) {
		addActionBinding(HtmlNode::getActionProperty, applyOnModel);
		return this;
	}

}
