package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.InputTextHtmlDomNode;

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
		return new InputTextHtmlDomNode();
	}

	public <M> HtmlInputText bindAction(Consumer<M> applyOnModel) {
		addActionBinding(InputTextHtmlDomNode::getEnterProperty, applyOnModel);
		return this;
	}

}
