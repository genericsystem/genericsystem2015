package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlDomNode.InputTextHtmlDomNode;
import org.genericsystem.distributed.ui.HtmlElement;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlInputText extends HtmlElement<HtmlInputText, InputTextHtmlDomNode> {

	public HtmlInputText(HtmlElement<?, ?> parent) {
		super(parent, InputTextHtmlDomNode.class);
	}

	@Override
	protected InputTextHtmlDomNode createNode(Object parent) {
		return new InputTextHtmlDomNode(getWebSocket());
	}

	public <M> HtmlInputText bindAction(Consumer<M> applyOnModel) {
		addActionBinding(InputTextHtmlDomNode::getEnterProperty, applyOnModel);
		return this;
	}

}
