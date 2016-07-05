package org.genericsystem.reactor.html;

import java.util.function.Consumer;

import org.genericsystem.reactor.ModelContext;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlInputText<M extends ModelContext> extends Tag<M> {

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
