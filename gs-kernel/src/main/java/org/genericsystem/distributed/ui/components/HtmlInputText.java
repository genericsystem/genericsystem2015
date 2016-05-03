package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.InputTextHtmlDomNode;
import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlInputText<M extends Model> extends HtmlElement<M, HtmlInputText<M>, InputTextHtmlDomNode> {

	public HtmlInputText(HtmlElement<?, ?, ?> parent) {
		super(parent, InputTextHtmlDomNode.class);
	}

	@Override
	protected InputTextHtmlDomNode createNode(Object parent) {
		return new InputTextHtmlDomNode();
	}

	public HtmlInputText<M> bindAction(Consumer<M> applyOnModel) {
		addActionBinding(InputTextHtmlDomNode::getEnterProperty, applyOnModel);
		return this;
	}

}
