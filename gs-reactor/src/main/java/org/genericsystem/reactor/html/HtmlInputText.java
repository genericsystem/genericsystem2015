package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlInputText<M extends Model> extends Element<M> {

	public HtmlInputText(Element<?> parent) {
		super(parent, "input");
	}

	@Override
	protected InputTextHtmlDomNode createNode(String parentId) {
		return new InputTextHtmlDomNode(parentId);
	}

	@Deprecated
	public void bindAction(Consumer<M> applyOnModel) {
		addActionBinding(InputTextHtmlDomNode::getEnterProperty, applyOnModel);
	}

}
