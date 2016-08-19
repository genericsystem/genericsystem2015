package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.HtmlDomNode.InputTextHtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlInputText<M extends Model> extends Tag<M> {

	public HtmlInputText(Tag<?> parent) {
		super(parent, "input");
	}

	@Override
	protected InputTextHtmlDomNode createNode(String parentId) {
		return new InputTextHtmlDomNode(parentId);
	}

	public void bindAction(Consumer<M> applyOnModel) {
		addActionBinding(InputTextHtmlDomNode::getEnterProperty, applyOnModel);
	}

}
