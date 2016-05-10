package org.genericsystem.reactor.html;

import java.util.function.Consumer;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.HtmlElement.InputTextHtmlDomNode;

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
