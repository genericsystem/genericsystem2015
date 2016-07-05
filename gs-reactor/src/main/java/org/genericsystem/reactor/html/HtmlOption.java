package org.genericsystem.reactor.html;

import org.genericsystem.reactor.ModelContext;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlOption<M extends ModelContext> extends Tag<M> {

	public HtmlOption(Tag<?> parent) {
		super(parent, "option");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

	// public void bindAction(Consumer<M> consumer) {
	// addActionBinding(ActionHtmlNode::getActionProperty, consumer);
	// }
}
