package org.genericsystem.reactor.html;

import org.genericsystem.reactor.CompositeModel;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlSelectTemplate<M extends CompositeModel, COMPONENT extends HtmlSelectTemplate<M, COMPONENT>>
		extends HtmlElement<M, COMPONENT, HtmlDomNode> {

	public HtmlSelectTemplate(HtmlElement<?, ?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode("select");
	}

	public static class HtmlSelect<M extends CompositeModel> extends HtmlSectionTemplate<M, HtmlSelect<M>> {

		public HtmlSelect(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

	}
}
